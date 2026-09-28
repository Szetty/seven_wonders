defmodule Helios.Games.GameServer do
  @moduledoc """
  One process per active game. Holds the engine resource, serialises
  submissions, appends every accepted action to `game_actions` *before*
  replying, and broadcasts `{:game_updated, seq}` (no state) on `"game:<id>"`.

  `init/1` rebuilds the engine from `seed` + `wonders` + the action log. If the
  log no longer replays (e.g. a rule change), the game is marked aborted and
  the process is not started (`:ignore`). An insert failure crashes the
  process on purpose: the supervisor restarts it and replay drops the
  unpersisted action, so memory is never ahead of the log.
  """
  use GenServer, restart: :transient

  import Ecto.Query
  require Logger

  alias Helios.{Core, Games, Repo}
  alias Helios.Games.{ActionCodec, Choice, Game, GameAction, GamePlayer}

  def start_link(opts) do
    game_id = Keyword.fetch!(opts, :game_id)
    GenServer.start_link(__MODULE__, opts, name: {:via, Registry, {Helios.Games.Registry, game_id}})
  end

  @impl true
  def init(opts) do
    game = Repo.get!(Game, Keyword.fetch!(opts, :game_id))
    idle_timeout = Keyword.fetch!(opts, :idle_timeout)

    player_ids =
      from(p in GamePlayer, where: p.game_id == ^game.id, order_by: p.seat, select: p.user_id)
      |> Repo.all()
      |> Enum.map(&to_string/1)

    actions = Repo.all(from a in GameAction, where: a.game_id == ^game.id, order_by: a.seq)
    warn_on_engine_version(game)

    with {:ok, wonders} <- ActionCodec.decode_wonders(game.wonders),
         {:ok, ref} <- Core.new_game(player_ids, wonders, game.seed),
         :ok <- replay(ref, actions) do
      state = %{
        game_id: game.id,
        lobby_id: game.lobby_id,
        ref: ref,
        seq: last_seq(actions),
        players: player_ids,
        status: game.status,
        idle_timeout: idle_timeout
      }

      state = maybe_finish(state)
      {:ok, state, state.idle_timeout}
    else
      {:error, reason} ->
        Logger.error("Game #{game.id} could not be rebuilt (#{inspect(reason)}); marking it aborted")
        abort(game)
        :ignore
    end
  end

  @impl true
  def handle_call({:submit, user_id, action}, _from, state) do
    do_submit(state, user_id, action)
  end

  def handle_call({:submit_choice, user_id, choice}, _from, state) do
    with {:ok, view} <- Core.view(state.ref, to_string(user_id)),
         {:ok, action} <- Choice.resolve(view, choice) do
      do_submit(state, user_id, action)
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end

  def handle_call({:view, user_id}, _from, state) do
    {:reply, Core.view(state.ref, to_string(user_id)), state, state.idle_timeout}
  end

  @impl true
  def handle_info(:timeout, state), do: {:stop, :normal, state}
  def handle_info(_message, state), do: {:noreply, state, state.idle_timeout}

  defp do_submit(state, user_id, action) do
    case Core.submit(state.ref, to_string(user_id), action) do
      :ok ->
        seq = state.seq + 1

        # Raises on failure -> crash -> restart + replay without this action.
        Repo.insert!(%GameAction{
          game_id: state.game_id,
          seq: seq,
          user_id: user_id,
          action: ActionCodec.encode(action)
        })

        state = maybe_finish(%{state | seq: seq})
        Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(state.game_id), {:game_updated, seq})
        {:reply, :ok, state, state.idle_timeout}

      {:error, reason} ->
        {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end

  defp replay(ref, actions) do
    Enum.reduce_while(actions, :ok, fn %GameAction{} = row, :ok ->
      with {:ok, action} <- ActionCodec.decode(row.action),
           :ok <- Core.submit(ref, to_string(row.user_id), action) do
        {:cont, :ok}
      else
        {:error, reason} -> {:halt, {:error, {:replay_failed, row.seq, reason}}}
      end
    end)
  end

  defp maybe_finish(%{status: "active"} = state) do
    {:ok, view} = Core.view(state.ref, hd(state.players))

    if view.phase.kind == :game_over do
      scores = Enum.map(view.scores, &stringify_keys/1)

      Game
      |> Repo.get!(state.game_id)
      |> Game.finish_changeset(%{"scores" => scores})
      |> Repo.update!()

      Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(state.game_id), {:game_finished})
      Phoenix.PubSub.broadcast(Helios.PubSub, "lobby:#{state.lobby_id}", {:members_changed})
      %{state | status: "finished"}
    else
      state
    end
  end

  defp maybe_finish(state), do: state

  defp abort(game) do
    Repo.update!(Game.abort_changeset(game))
    Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(game.id), {:game_aborted})
    Phoenix.PubSub.broadcast(Helios.PubSub, "lobby:#{game.lobby_id}", {:members_changed})
  end

  defp warn_on_engine_version(%Game{engine_version: version} = game) do
    current = Core.game_settings().engine_version

    if version != current do
      Logger.warning(
        "Game #{game.id} was created with engine version #{version}, running #{current}; replaying anyway"
      )
    end
  end

  defp last_seq([]), do: 0
  defp last_seq(actions), do: List.last(actions).seq

  defp stringify_keys(map), do: Map.new(map, fn {key, value} -> {Atom.to_string(key), value} end)
end

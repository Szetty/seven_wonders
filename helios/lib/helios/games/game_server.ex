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

  Submissions carry a `source` (`:human | :ai | :random`); random ones are
  kept as `Helios.Games.RandomMoves` keys, rebuilt from the log on init and
  returned by `:snapshot`. Starts one `Helios.AI.Player` per AI seat of an
  active game.
  """
  use GenServer, restart: :transient

  require Logger

  alias Helios.{Core, Games, Repo}
  alias Helios.Games.{ActionCodec, Actor, Choice, Game, GameAction, Replay}

  def start_link(opts) do
    game_id = Keyword.fetch!(opts, :game_id)

    GenServer.start_link(__MODULE__, opts,
      name: {:via, Registry, {Helios.Games.Registry, game_id}}
    )
  end

  @doc "Direct call for processes holding the server pid (AI players): no ensure_started, no retry."
  def view(server, actor), do: GenServer.call(server, {:view, actor})

  @doc "Direct call for processes holding the server pid (AI players)."
  def submit_choice(server, actor, choice, source),
    do: GenServer.call(server, {:submit_choice, actor, choice, source})

  @impl true
  def init(opts) do
    game = Repo.get!(Game, Keyword.fetch!(opts, :game_id))
    idle_timeout = Keyword.fetch!(opts, :idle_timeout)
    actions = Replay.actions(game.id)
    warn_on_engine_version(game)

    case Replay.rebuild_with_random(game, actions, fn _ref -> nil end, fn _row, _ref, acc ->
           acc
         end) do
      {:ok, ref, nil, random} ->
        state = %{
          game_id: game.id,
          lobby_id: game.lobby_id,
          ref: ref,
          seq: last_seq(actions),
          players: Replay.player_ids(game.id),
          random: random,
          status: game.status,
          idle_timeout: idle_timeout
        }

        state = maybe_finish(state)
        if state.status == "active", do: start_ai_players(game.id)
        {:ok, state, state.idle_timeout}

      {:error, reason} ->
        Logger.error(
          "Game #{game.id} could not be rebuilt (#{inspect(reason)}); marking it aborted"
        )

        abort(game)
        :ignore
    end
  end

  @impl true
  def handle_call({:submit, actor, action, source}, _from, state) do
    do_submit(state, actor, action, source)
  end

  def handle_call({:submit_choice, actor, choice, source}, _from, state) do
    with {:ok, view} <- Core.view(state.ref, Actor.player_id(actor)),
         {:ok, action} <- Choice.resolve(view, choice) do
      do_submit(state, actor, action, source)
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end

  def handle_call({:view, actor}, _from, state) do
    {:reply, Core.view(state.ref, Actor.player_id(actor)), state, state.idle_timeout}
  end

  def handle_call(:history, _from, state) do
    {:reply, Core.history(state.ref), state, state.idle_timeout}
  end

  def handle_call({:snapshot, actor}, _from, state) do
    with {:ok, view} <- Core.view(state.ref, Actor.player_id(actor)),
         {:ok, history} <- Core.history(state.ref) do
      {:reply, {:ok, %{view: view, history: history, random: state.random}}, state,
       state.idle_timeout}
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end

  @impl true
  def handle_info(:timeout, state), do: {:stop, :normal, state}
  def handle_info(_message, state), do: {:noreply, state, state.idle_timeout}

  defp do_submit(state, actor, action, source) do
    player = Actor.player_id(actor)

    with {:ok, %{phase: phase}} <- Core.view(state.ref, player),
         :ok <- Core.submit(state.ref, player, action) do
      seq = state.seq + 1

      # Raises on failure -> crash -> restart + replay without this action.
      Repo.insert!(
        struct(
          GameAction,
          Map.merge(Actor.row_fields(actor), %{
            game_id: state.game_id,
            seq: seq,
            source: Atom.to_string(source),
            action: ActionCodec.encode(action)
          })
        )
      )

      random = Helios.Games.RandomMoves.record(state.random, source, phase, player)
      state = maybe_finish(%{state | seq: seq, random: random})
      Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(state.game_id), {:game_updated, seq})
      {:reply, :ok, state, state.idle_timeout}
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
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

  # One Helios.AI.Player per AI seat; each monitors this server and stops with it.
  defp start_ai_players(game_id) do
    if Keyword.get(Application.get_env(:helios, Helios.AI, []), :players, true) do
      for %{ai_model: model, seat: seat} <- Replay.seats(game_id), is_binary(model) do
        {:ok, _pid} =
          DynamicSupervisor.start_child(
            Helios.AI.Supervisor,
            {Helios.AI.Player, game_id: game_id, seat: seat, model_id: model, server: self()}
          )
      end
    end

    :ok
  end

  defp last_seq([]), do: 0
  defp last_seq(actions), do: List.last(actions).seq

  defp stringify_keys(map), do: Map.new(map, fn {key, value} -> {Atom.to_string(key), value} end)
end

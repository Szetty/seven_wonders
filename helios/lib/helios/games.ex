defmodule Helios.Games do
  @moduledoc """
  Game orchestration: persistence of games, seats and the action log, and the
  per-game `Helios.Games.GameServer` processes that wrap the Rust engine.
  """
  import Ecto.Query

  alias Ecto.Multi
  alias Helios.{Core, Lobbies, Repo}
  alias Helios.Accounts.Scope
  alias Helios.Games.{ActionCodec, Game, GamePlayer, GameServer, Replay}
  alias HeliosWeb.Presence

  @min_players 3
  @max_players 7

  @registry Helios.Games.Registry
  @supervisor Helios.Games.Supervisor

  @spec topic(Ecto.UUID.t()) :: String.t()
  def topic(game_id), do: "game:#{game_id}"

  @spec config() :: keyword()
  def config, do: Application.get_env(:helios, __MODULE__, [])

  @spec fetch_game(term()) :: {:ok, Game.t()} | {:error, :not_found}
  def fetch_game(game_id) do
    with {:ok, uuid} <- Ecto.UUID.cast(game_id),
         %Game{} = game <- Repo.get(Game, uuid) do
      {:ok, game}
    else
      _ -> {:error, :not_found}
    end
  end

  @spec players(Game.t()) :: [GamePlayer.t()]
  def players(%Game{id: id}) do
    Repo.all(from p in GamePlayer, where: p.game_id == ^id, order_by: p.seat, preload: :user)
  end

  @spec seated?(Game.t(), integer()) :: boolean()
  def seated?(%Game{id: id}, user_id) do
    Repo.exists?(from p in GamePlayer, where: p.game_id == ^id and p.user_id == ^user_id)
  end

  @spec active_game_for_lobby(Ecto.UUID.t()) :: Game.t() | nil
  def active_game_for_lobby(lobby_id) do
    Repo.one(from g in Game, where: g.lobby_id == ^lobby_id and g.status == "active")
  end

  @spec finished_games_for(Helios.Accounts.User.t(), pos_integer()) :: [Game.t()]
  def finished_games_for(%{id: user_id}, limit \\ 50) do
    Repo.all(
      from g in Game,
        join: p in GamePlayer,
        on: p.game_id == g.id and p.user_id == ^user_id,
        where: g.status == "finished",
        order_by: [desc: g.finished_at, desc: g.inserted_at],
        limit: ^limit,
        preload: [players: :user]
    )
  end

  @spec connected_user_ids(String.t()) :: MapSet.t(String.t())
  def connected_user_ids(topic), do: topic |> Presence.list() |> Map.keys() |> MapSet.new()

  @doc "The owner plus invitees currently connected to the lobby, in members order."
  def eligible_players(lobby) do
    connected = connected_user_ids("lobby:#{lobby.id}")

    for %{user: user, leader?: leader?} <- Lobbies.members(lobby),
        leader? or MapSet.member?(connected, to_string(user.id)),
        do: user
  end

  @spec start_blocker(Game.t() | nil, non_neg_integer()) :: String.t() | nil
  def start_blocker(%Game{}, _count), do: error_message(:game_in_progress)

  def start_blocker(nil, count) do
    case check_player_count(count) do
      :ok -> nil
      {:error, reason} -> error_message(reason)
    end
  end

  @spec start_game(Scope.t(), Lobbies.Lobby.t()) :: {:ok, Game.t()} | {:error, term()}
  def start_game(%Scope{user: user}, lobby) do
    players = eligible_players(lobby)

    with :ok <- check_leader(lobby, user),
         :ok <- check_player_count(length(players)),
         :ok <- check_no_active_game(lobby),
         player_ids = Enum.map(players, &to_string(&1.id)),
         wonders = configured_wonders(length(players)),
         seed = new_seed(),
         :ok <- validate_setup(player_ids, wonders, seed),
         {:ok, game} <- insert_game(lobby, players, seed, wonders),
         {:ok, game} <- boot(game) do
      Phoenix.PubSub.broadcast(Helios.PubSub, "lobby:#{lobby.id}", {:game_started, game.id})
      {:ok, game}
    end
  end

  @spec error_message(term()) :: String.t()
  def error_message(:unknown_player), do: "You are not seated at this game"
  def error_message(:not_your_turn), do: "It's not your turn"
  def error_message(:game_over), do: "The game is over"
  def error_message(:card_not_in_hand), do: "That card is no longer in your hand"
  def error_message(:card_not_in_discard), do: "That card is not in the discard pile"
  def error_message(:already_built), do: "You already built that structure"
  def error_message(:cannot_afford), do: "You can't afford that"
  def error_message(:invalid_payment), do: "That payment is no longer valid"
  def error_message(:no_wonder_stage_left), do: "Your wonder is complete"
  def error_message(:free_build_unavailable), do: "Your free build is not available"
  def error_message(:action_not_allowed_now), do: "You can't do that right now"
  def error_message(:not_leader), do: "Only the table leader can start a game"
  def error_message(:not_enough_players), do: "Need at least 3 connected players"
  def error_message(:too_many_players), do: "At most 7 players can play"
  def error_message(:game_in_progress), do: "A game is already running at this table"
  def error_message(:not_found), do: "That game does not exist"
  def error_message(:not_active), do: "That game has already finished"
  def error_message(:aborted), do: "This game was aborted"
  def error_message(:unauthorized), do: "You are not seated at this game"
  def error_message(:server_restarted), do: "The game was interrupted, please try again"
  def error_message(:invalid_choice), do: "That choice is not available"
  def error_message({:setup_failed, _reason}), do: "The game could not be set up"
  def error_message(_reason), do: "Something went wrong, please try again"

  defp check_leader(lobby, user) do
    if lobby.owner_id == user.id, do: :ok, else: {:error, :not_leader}
  end

  defp check_player_count(count) when count < @min_players, do: {:error, :not_enough_players}
  defp check_player_count(count) when count > @max_players, do: {:error, :too_many_players}
  defp check_player_count(_count), do: :ok

  defp check_no_active_game(lobby) do
    if active_game_for_lobby(lobby.id), do: {:error, :game_in_progress}, else: :ok
  end

  defp configured_wonders(count) do
    case Keyword.get(config(), :wonders) do
      nil -> :random
      wonders -> {:explicit, Enum.take(wonders, count)}
    end
  end

  defp new_seed, do: Keyword.get(config(), :fixed_seed) || :rand.uniform(2 ** 63) - 1

  # Cheap dry run so an engine rejection never leaves a row or a crash report behind.
  defp validate_setup(player_ids, wonders, seed) do
    case Core.new_game(player_ids, wonders, seed) do
      {:ok, _ref} -> :ok
      {:error, reason} -> {:error, {:setup_failed, reason}}
    end
  end

  defp insert_game(lobby, players, seed, wonders) do
    attrs = %{
      seed: seed,
      engine_version: Core.game_settings().engine_version,
      wonders: ActionCodec.encode_wonders(wonders)
    }

    Multi.new()
    |> Multi.insert(:game, Game.create_changeset(%Game{lobby_id: lobby.id}, attrs))
    |> Multi.insert_all(:players, GamePlayer, fn %{game: game} ->
      Enum.with_index(players, fn user, seat ->
        %{game_id: game.id, user_id: user.id, seat: seat}
      end)
    end)
    |> Repo.transaction()
    |> case do
      {:ok, %{game: game}} ->
        {:ok, game}

      {:error, :game, %Ecto.Changeset{errors: errors} = changeset, _changes} ->
        if Keyword.has_key?(errors, :lobby_id),
          do: {:error, :game_in_progress},
          else: {:error, changeset}
    end
  end

  defp boot(game) do
    case ensure_started(game.id) do
      {:ok, _pid} ->
        {:ok, game}

      {:error, reason} ->
        Repo.delete!(game)
        {:error, reason}
    end
  end

  @spec ensure_started(term()) :: {:ok, pid()} | {:error, term()}
  def ensure_started(game_id) do
    with {:ok, uuid} <- cast_id(game_id) do
      case Registry.lookup(@registry, uuid) do
        [{pid, _}] -> if Process.alive?(pid), do: {:ok, pid}, else: start_server(uuid)
        [] -> start_server(uuid)
      end
    end
  end

  @spec submit(term(), integer(), term()) :: :ok | {:error, term()}
  def submit(game_id, user_id, action), do: call(game_id, {:submit, user_id, action})

  @spec submit_choice(term(), integer(), Helios.Games.Choice.t()) :: :ok | {:error, term()}
  def submit_choice(game_id, user_id, %{} = choice),
    do: call(game_id, {:submit_choice, user_id, choice})

  @spec view(term(), integer()) :: {:ok, map()} | {:error, term()}
  def view(game_id, user_id), do: call(game_id, {:view, user_id})

  @spec history(term()) :: {:ok, [term()]} | {:error, term()}
  def history(game_id), do: call(game_id, :history)

  @doc """
  The viewer's view and the public history read in one `GameServer` call, so
  the two can never be split by a concurrent submit the way separate
  `view/2` + `history/1` calls can.
  """
  @spec snapshot(term(), integer()) :: {:ok, %{view: map(), history: [term()]}} | {:error, term()}
  def snapshot(game_id, user_id), do: call(game_id, {:snapshot, user_id})

  @doc """
  Rebuilds a finished game from its log in a throwaway engine: the viewer's
  final view, the public history, and one frame per turn (public boards plus
  the viewer's own hand and final choice that turn, the discard-pile size and,
  in the viewer's own build-from-discard turn only, the buildable discards).
  `{:error, :not_finished}` if the log stops short of game over.
  """
  @spec replay(Game.t(), integer()) ::
          {:ok, %{view: map(), history: [term()], frames: [map()]}} | {:error, term()}
  def replay(%Game{} = game, user_id) do
    me = to_string(user_id)
    init = fn ref -> %{current: frame_snapshot(ref, me), done: [], turns: 0, my_last: nil} end
    step = fn row, ref, acc -> frame_step(row, ref, acc, me) end

    with {:ok, ref, acc} <- Replay.rebuild(game, Replay.actions(game.id), init, step),
         {:ok, %{phase: %{kind: :game_over}} = view} <- Core.view(ref, me),
         {:ok, history} <- Core.history(ref) do
      {:ok, %{view: view, history: history, frames: Enum.reverse(acc.done, [acc.current])}}
    else
      {:ok, _view} -> {:error, :not_finished}
      {:error, reason} -> {:error, reason}
    end
  end

  defp frame_step(row, ref, acc, me) do
    acc =
      if to_string(row.user_id) == me do
        {:ok, action} = ActionCodec.decode(row.action)
        %{acc | my_last: action}
      else
        acc
      end

    {:ok, history} = Core.history(ref)
    turn_indexes = for {{:turn, _}, index} <- Enum.with_index(history), do: index

    if length(turn_indexes) > acc.turns do
      ended = %{acc.current | my_choice: acc.my_last, event_index: List.last(turn_indexes)}

      %{
        acc
        | done: [ended | acc.done],
          current: frame_snapshot(ref, me),
          turns: length(turn_indexes),
          my_last: nil
      }
    else
      acc
    end
  end

  defp frame_snapshot(ref, me) do
    {:ok, view} = Core.view(ref, me)

    %{
      players: view.players,
      hand: Enum.map(view.hand, & &1.name),
      phase: view.phase,
      # Non-nil only in the viewer's own build-from-discard turn: the engine
      # fills it for that player alone, so the replay shows the discards
      # exactly to whoever saw them live.
      discard_pile: view.discard_pile,
      discard_count: view.discard_count,
      my_choice: nil,
      event_index: nil
    }
  end

  defp cast_id(game_id) do
    case Ecto.UUID.cast(game_id) do
      {:ok, uuid} -> {:ok, uuid}
      :error -> {:error, :not_found}
    end
  end

  defp start_server(game_id) do
    case fetch_game(game_id) do
      {:error, :not_found} ->
        {:error, :not_found}

      {:ok, %Game{status: "aborted"}} ->
        {:error, :aborted}

      {:ok, %Game{status: "finished"}} ->
        {:error, :not_active}

      {:ok, %Game{status: "active", id: id}} ->
        spec = {GameServer, game_id: id, idle_timeout: idle_timeout()}

        case DynamicSupervisor.start_child(@supervisor, spec) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          :ignore -> {:error, :aborted}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  # A server that stopped between lookup and call (idle timeout) is retried once;
  # a server that crashed while handling the call is reported, never retried.
  defp call(game_id, message, retries \\ 1) do
    with {:ok, pid} <- ensure_started(game_id) do
      try do
        GenServer.call(pid, message)
      catch
        :exit, {reason, _} when reason in [:noproc, :normal] and retries > 0 ->
          call(game_id, message, retries - 1)

        :exit, _reason ->
          {:error, :server_restarted}
      end
    end
  end

  defp idle_timeout, do: Keyword.get(config(), :idle_timeout, :timer.minutes(30))
end

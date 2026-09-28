defmodule Helios.Games do
  @moduledoc """
  Game orchestration: persistence of games, seats and the action log, and the
  per-game `Helios.Games.GameServer` processes that wrap the Rust engine.
  """
  import Ecto.Query

  alias Helios.Repo
  alias Helios.Games.{Game, GamePlayer, GameServer}

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
  def submit_choice(game_id, user_id, %{} = choice), do: call(game_id, {:submit_choice, user_id, choice})

  @spec view(term(), integer()) :: {:ok, map()} | {:error, term()}
  def view(game_id, user_id), do: call(game_id, {:view, user_id})

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

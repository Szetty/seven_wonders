defmodule Helios.Games.Replay do
  @moduledoc """
  Rebuilds a game's engine from `seed` + `wonders` + the action log. Used by
  `GameServer.init/1` and, for finished games, by `Helios.Games.replay/2`
  (a throwaway engine, never registered as a GameServer).
  """
  import Ecto.Query

  alias Helios.{Core, Repo}
  alias Helios.Games.{ActionCodec, Game, GameAction, GamePlayer}

  @spec player_ids(Ecto.UUID.t()) :: [String.t()]
  def player_ids(game_id) do
    from(p in GamePlayer, where: p.game_id == ^game_id, order_by: p.seat, select: p.user_id)
    |> Repo.all()
    |> Enum.map(&to_string/1)
  end

  @spec actions(Ecto.UUID.t()) :: [GameAction.t()]
  def actions(game_id),
    do: Repo.all(from a in GameAction, where: a.game_id == ^game_id, order_by: a.seq)

  @doc """
  A fresh engine with every row of `actions` submitted in order. `init` sees
  the fresh engine and returns the accumulator; `step` sees each row with the
  engine just after it was applied.
  """
  @spec rebuild(Game.t(), [GameAction.t()], (Core.game() -> acc), (GameAction.t(),
                                                                   Core.game(),
                                                                   acc ->
                                                                     acc)) ::
          {:ok, Core.game(), acc} | {:error, term()}
        when acc: term()
  def rebuild(%Game{} = game, actions, init, step) do
    with {:ok, wonders} <- ActionCodec.decode_wonders(game.wonders),
         {:ok, ref} <- Core.new_game(player_ids(game.id), wonders, game.seed) do
      actions
      |> Enum.reduce_while({:ok, init.(ref)}, fn %GameAction{} = row, {:ok, acc} ->
        with {:ok, action} <- ActionCodec.decode(row.action),
             :ok <- Core.submit(ref, to_string(row.user_id), action) do
          {:cont, {:ok, step.(row, ref, acc)}}
        else
          {:error, reason} -> {:halt, {:error, {:replay_failed, row.seq, reason}}}
        end
      end)
      |> case do
        {:ok, acc} -> {:ok, ref, acc}
        error -> error
      end
    end
  end
end

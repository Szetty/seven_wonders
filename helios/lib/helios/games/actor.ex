defmodule Helios.Games.Actor do
  @moduledoc """
  Who acts in a game: a user id (integer) or `{:ai, seat}`. The engine knows
  players by string id: `"<user_id>"` or `"ai-<seat>"` (a hyphen, so ids stay
  valid in DOM ids and CSS selectors).
  """

  alias Helios.Games.{GameAction, GamePlayer}

  @type t :: integer() | {:ai, non_neg_integer()}

  @spec player_id(t()) :: String.t()
  def player_id({:ai, seat}) when is_integer(seat), do: "ai-#{seat}"
  def player_id(user_id) when is_integer(user_id), do: Integer.to_string(user_id)

  @spec from_player_id(String.t()) :: t()
  def from_player_id("ai-" <> seat), do: {:ai, String.to_integer(seat)}
  def from_player_id(id) when is_binary(id), do: String.to_integer(id)

  @spec of_seat(GamePlayer.t()) :: t()
  def of_seat(%GamePlayer{ai_model: nil, user_id: user_id}), do: user_id
  def of_seat(%GamePlayer{seat: seat}), do: {:ai, seat}

  @spec of_row(GameAction.t()) :: t()
  def of_row(%GameAction{user_id: nil, seat: seat}), do: {:ai, seat}
  def of_row(%GameAction{user_id: user_id}), do: user_id

  @spec row_fields(t()) :: %{user_id: integer() | nil, seat: non_neg_integer() | nil}
  def row_fields({:ai, seat}), do: %{user_id: nil, seat: seat}
  def row_fields(user_id) when is_integer(user_id), do: %{user_id: user_id, seat: nil}
end

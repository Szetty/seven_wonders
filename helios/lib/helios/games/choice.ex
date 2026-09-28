defmodule Helios.Games.Choice do
  @moduledoc """
  A player's choice as sent by the browser: a card, a kind and, for trades, the
  index of a payment option in the server's current view. Clients never send
  payments; `resolve/2` looks the payment up in the view.
  """

  @kinds ~w(build wonder_stage discard build_free build_from_discard)
  @empty_payment %{west: [], east: []}

  @type t :: %{card: String.t(), kind: String.t(), option: non_neg_integer()}

  @spec parse(map()) :: {:ok, t()} | {:error, :invalid_choice}
  def parse(%{"card" => card, "kind" => kind} = params) when is_binary(card) and kind in @kinds do
    case params |> Map.get("option", "0") |> to_string() |> Integer.parse() do
      {option, ""} when option >= 0 -> {:ok, %{card: card, kind: kind, option: option}}
      _other -> {:error, :invalid_choice}
    end
  end

  def parse(_params), do: {:error, :invalid_choice}

  @spec resolve(map(), t()) :: {:ok, Helios.Games.ActionCodec.action()} | {:error, atom()}
  def resolve(_view, %{kind: "discard", card: card}), do: {:ok, {:discard, card}}
  def resolve(_view, %{kind: "build_free", card: card}), do: {:ok, {:build_free, card}}
  def resolve(_view, %{kind: "build_from_discard", card: card}), do: {:ok, {:build_from_discard, card}}

  def resolve(view, %{kind: "build", card: card, option: option}) do
    with {:ok, hand_card} <- hand_card(view, card),
         {:ok, payment} <- payment(hand_card.build, option) do
      {:ok, {:build, %{card: card, payment: payment}}}
    end
  end

  def resolve(view, %{kind: "wonder_stage", card: card, option: option}) do
    with {:ok, hand_card} <- hand_card(view, card),
         {:ok, payment} <- payment(hand_card.wonder_stage, option) do
      {:ok, {:build_wonder_stage, %{card: card, payment: payment}}}
    end
  end

  def resolve(_view, _choice), do: {:error, :invalid_choice}

  @spec available?(term()) :: boolean()
  def available?(:free), do: true
  def available?({:coins, _}), do: true
  def available?({:trade, [_ | _]}), do: true
  def available?(_option), do: false

  defp hand_card(view, card) do
    case Enum.find(view.hand, &(&1.name == card)) do
      nil -> {:error, :card_not_in_hand}
      hand_card -> {:ok, hand_card}
    end
  end

  defp payment({:unavailable, reason}, _option), do: {:error, reason}
  defp payment(:free, _option), do: {:ok, @empty_payment}
  defp payment({:coins, _}, _option), do: {:ok, @empty_payment}

  defp payment({:trade, options}, option) when is_integer(option) and option >= 0 do
    case Enum.at(options, option) do
      nil -> {:error, :invalid_payment}
      %{payment: payment} -> {:ok, payment}
    end
  end

  defp payment(_build_option, _option), do: {:error, :invalid_payment}
end

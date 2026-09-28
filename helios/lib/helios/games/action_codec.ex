defmodule Helios.Games.ActionCodec do
  @moduledoc """
  Converts engine action terms and wonder selections to JSON-safe maps for the
  `game_actions.action` and `games.wonders` columns, and back.

  Decoding only maps whitelisted strings to atoms; it never creates atoms.
  """

  @resources %{
    "wood" => :wood,
    "stone" => :stone,
    "ore" => :ore,
    "clay" => :clay,
    "glass" => :glass,
    "loom" => :loom,
    "papyrus" => :papyrus
  }
  @resource_names Map.new(@resources, fn {name, atom} -> {atom, name} end)
  @paid_types %{"build" => :build, "build_wonder_stage" => :build_wonder_stage}
  @card_types %{
    "discard" => :discard,
    "build_free" => :build_free,
    "build_from_discard" => :build_from_discard
  }
  @sides %{"a" => :a, "b" => :b}

  @type resource :: :wood | :stone | :ore | :clay | :glass | :loom | :papyrus
  @type payment :: %{west: [{resource(), pos_integer()}], east: [{resource(), pos_integer()}]}
  @type action ::
          {:build, %{card: String.t(), payment: payment()}}
          | {:build_wonder_stage, %{card: String.t(), payment: payment()}}
          | {:discard, String.t()}
          | {:build_free, String.t()}
          | {:build_from_discard, String.t()}
  @type wonders :: :random | {:explicit, [{String.t(), :a | :b}]}

  @spec encode(action()) :: map()
  def encode({type, %{card: card, payment: payment}})
      when type in [:build, :build_wonder_stage] and is_binary(card) do
    %{"type" => Atom.to_string(type), "card" => card, "payment" => encode_payment(payment)}
  end

  def encode({type, card}) when type in [:discard, :build_free, :build_from_discard] and is_binary(card) do
    %{"type" => Atom.to_string(type), "card" => card}
  end

  @spec decode(term()) :: {:ok, action()} | {:error, :invalid_action}
  def decode(%{"type" => type, "card" => card, "payment" => payment})
      when is_map_key(@paid_types, type) and is_binary(card) do
    with {:ok, payment} <- decode_payment(payment) do
      {:ok, {Map.fetch!(@paid_types, type), %{card: card, payment: payment}}}
    end
  end

  def decode(%{"type" => type, "card" => card}) when is_map_key(@card_types, type) and is_binary(card) do
    {:ok, {Map.fetch!(@card_types, type), card}}
  end

  def decode(_other), do: {:error, :invalid_action}

  @spec encode_wonders(wonders()) :: nil | map()
  def encode_wonders(:random), do: nil

  def encode_wonders({:explicit, list}) do
    %{"explicit" => Enum.map(list, fn {name, side} -> [name, Atom.to_string(side)] end)}
  end

  @spec decode_wonders(nil | map()) :: {:ok, wonders()} | {:error, :invalid_wonders}
  def decode_wonders(nil), do: {:ok, :random}

  def decode_wonders(%{"explicit" => list}) when is_list(list) do
    list
    |> Enum.reduce_while({:ok, []}, fn
      [name, side], {:ok, acc} when is_binary(name) and is_map_key(@sides, side) ->
        {:cont, {:ok, [{name, Map.fetch!(@sides, side)} | acc]}}

      _other, _acc ->
        {:halt, {:error, :invalid_wonders}}
    end)
    |> case do
      {:ok, acc} -> {:ok, {:explicit, Enum.reverse(acc)}}
      error -> error
    end
  end

  def decode_wonders(_other), do: {:error, :invalid_wonders}

  defp encode_payment(%{west: west, east: east}) do
    %{"west" => encode_purchases(west), "east" => encode_purchases(east)}
  end

  defp encode_purchases(purchases) do
    Enum.map(purchases, fn {resource, count} -> [Map.fetch!(@resource_names, resource), count] end)
  end

  defp decode_payment(%{"west" => west, "east" => east}) do
    with {:ok, west} <- decode_purchases(west),
         {:ok, east} <- decode_purchases(east) do
      {:ok, %{west: west, east: east}}
    end
  end

  defp decode_payment(_other), do: {:error, :invalid_action}

  defp decode_purchases(purchases) when is_list(purchases) do
    purchases
    |> Enum.reduce_while({:ok, []}, fn
      [name, count], {:ok, acc}
      when is_map_key(@resources, name) and is_integer(count) and count > 0 ->
        {:cont, {:ok, [{Map.fetch!(@resources, name), count} | acc]}}

      _other, _acc ->
        {:halt, {:error, :invalid_action}}
    end)
    |> case do
      {:ok, acc} -> {:ok, Enum.reverse(acc)}
      error -> error
    end
  end

  defp decode_purchases(_other), do: {:error, :invalid_action}
end

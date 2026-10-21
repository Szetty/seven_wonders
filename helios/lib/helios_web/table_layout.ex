defmodule HeliosWeb.TableLayout do
  @moduledoc """
  Pure seat geometry for the table ring: slot positions, neighbour arrows,
  roles and the closed ring path, returned in narrow order (W, me, E, …).
  """

  @type arrow :: String.t()
  @type role :: :me | :west | :east | :opposite | nil
  @type seat :: %{
          player: map(),
          role: role(),
          west: String.t(),
          east: String.t(),
          ring: %{left: number(), top: number(), width: number()},
          ring_arrows: {arrow(), arrow()},
          list_arrows: {arrow(), arrow()},
          panel_align: :left | :right
        }

  @me_left 30
  @me_width 40
  @side_width 22
  @compact_height 15
  @me_reserve 48
  @gap 1
  @nominal_width 96
  @bottom @me_reserve + @gap

  @slots %{
    3 => %{0 => {@me_left, 0}, 1 => {78, 4}, 2 => {0, 4}},
    4 => %{0 => {@me_left, 0}, 1 => {78, 24}, 2 => {39, 49}, 3 => {0, 24}},
    5 => %{
      0 => {@me_left, 0},
      1 => {78, 18},
      2 => {51, 49},
      3 => {27, 49},
      4 => {0, 18}
    },
    6 => %{
      0 => {@me_left, 0},
      1 => {78, 8},
      2 => {78, 30},
      3 => {39, 49},
      4 => {0, 30},
      5 => {0, 8}
    },
    7 => %{
      0 => {@me_left, 0},
      1 => {78, 6},
      2 => {78, 27},
      3 => {51, 49},
      4 => {27, 49},
      5 => {0, 27},
      6 => {0, 6}
    }
  }

  @arrow_ring ["→", "↘", "↓", "↙", "←", "↖", "↑", "↗"]

  @spec build([map()], String.t()) :: %{
          seats: [seat()],
          ring_height: number(),
          ring_path: String.t()
        }
  def build(players, me) when is_list(players) do
    n = length(players)

    if n not in 3..7 do
      raise ArgumentError, "table size must be in 3..7, got: #{n}"
    end

    me_index = Enum.find_index(players, &(Map.get(&1, :name) == me))

    if is_nil(me_index) do
      raise ArgumentError, "viewer #{inspect(me)} is not seated at the table"
    end

    slots = Map.fetch!(@slots, n)
    ordered = (players ++ players) |> Enum.drop(Integer.mod(me_index - 1, n)) |> Enum.take(n)

    base =
      ordered
      |> Enum.with_index()
      |> Enum.map(fn {player, index} ->
        k = Integer.mod(index - 1, n)
        {left, top} = Map.fetch!(slots, k)

        %{
          player: player,
          index: index,
          k: k,
          left: left,
          top: top,
          width: if(k == 0, do: @me_width, else: @side_width)
        }
      end)

    at = fn i -> Enum.at(base, Integer.mod(i, n)) end

    anchor = fn s ->
      if s.k == 0, do: {50, 7}, else: {s.left + @side_width / 2, s.top + @compact_height / 2}
    end

    seats =
      Enum.map(base, fn s ->
        west = at.(s.index - 1)
        east = at.(s.index + 1)
        from = anchor.(s)

        %{
          player: s.player,
          role: role(s.k, n),
          west: Map.get(west.player, :name),
          east: Map.get(east.player, :name),
          ring: %{left: s.left, top: s.top, width: s.width},
          ring_arrows: {arrow(from, anchor.(west)), arrow(from, anchor.(east))},
          list_arrows:
            {if(s.index == 0, do: "↧", else: "↑"), if(s.index == n - 1, do: "↥", else: "↓")},
          panel_align: if(s.left > 50, do: :right, else: :left)
        }
      end)

    anchors = base |> Enum.sort_by(& &1.k) |> Enum.map(anchor)

    ring_height =
      if n == 3, do: 4 + @compact_height, else: @bottom + @compact_height

    %{seats: seats, ring_height: ring_height, ring_path: ring_path(anchors)}
  end

  @spec label(seat()) :: String.t()
  def label(%{role: role}) do
    case role do
      :me -> "You"
      :west -> "West"
      :east -> "East"
      :opposite -> "Opposite"
      nil -> ""
    end
  end

  defp role(0, _n), do: :me
  defp role(1, _n), do: :east
  defp role(k, n) when k == n - 1, do: :west
  defp role(k, n) when rem(n, 2) == 0 and k == div(n, 2), do: :opposite
  defp role(_k, _n), do: nil

  defp arrow({ax, ay}, {bx, by}) do
    dx = (bx - ax) * @nominal_width / 100
    dy = by - ay
    degrees = :math.atan2(dy, dx) * 180 / :math.pi()
    Enum.at(@arrow_ring, Integer.mod(round(degrees / 45), 8))
  end

  defp ring_path(anchors) do
    n = length(anchors)
    at = fn i -> Enum.at(anchors, Integer.mod(i, n)) end

    segments =
      for i <- 0..(n - 1) do
        {px, py} = at.(i - 1)
        {x, y} = at.(i)
        {nx, ny} = at.(i + 1)
        {x2, y2} = at.(i + 2)

        c1x = x + (nx - px) / 6
        c1y = y + (ny - py) / 6
        c2x = nx - (x2 - x) / 6
        c2y = ny - (y2 - y) / 6

        " C #{fmt(c1x)} #{fmt(c1y)} #{fmt(c2x)} #{fmt(c2y)} #{fmt(nx)} #{fmt(ny)}"
      end

    {x0, y0} = hd(anchors)
    "M #{fmt(x0)} #{fmt(y0)}" <> Enum.join(segments) <> " Z"
  end

  defp fmt(number), do: :erlang.float_to_binary(number * 1.0, decimals: 2)
end

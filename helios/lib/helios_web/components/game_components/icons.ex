defmodule HeliosWeb.GameComponents.Icons do
  @moduledoc "Game symbols that heroicons has no match for."
  use HeliosWeb, :html

  attr :class, :any, default: "size-5"
  attr :rest, :global

  @doc """
  The military symbol printed on military cards: a round red shield with a dark
  rim, a light Greek-key border (one dashed ring) and two crossed swords.
  Drawn on a 24-unit grid with no stroke under 1.7 units so it reads at `size-3.5`.
  """
  def military_icon(assigns) do
    ~H"""
    <svg
      data-icon="military"
      viewBox="0 0 24 24"
      class={@class}
      aria-hidden="true"
      {@rest}
    >
      <circle
        data-part="rim"
        cx="12"
        cy="12"
        r="10.5"
        fill="#b91c1c"
        stroke="#1c1917"
        stroke-width="2.0"
      />
      <circle
        data-part="border"
        cx="12"
        cy="12"
        r="7.75"
        fill="none"
        stroke="#fef3c7"
        stroke-width="1.75"
        stroke-dasharray="1.9 1.4"
      />
      <g data-part="sword" stroke-linecap="round">
        <path data-part="blade" d="M5.5 5.5 L15 15" stroke="#e7e5e4" stroke-width="2.25" />
        <path
          data-part="hilt"
          d="M13 17 L17 13 M15.25 15.25 L17.75 17.75"
          stroke="#44403c"
          stroke-width="2.25"
        />
      </g>
      <g data-part="sword" stroke-linecap="round">
        <path data-part="blade" d="M18.5 5.5 L9 15" stroke="#e7e5e4" stroke-width="2.25" />
        <path
          data-part="hilt"
          d="M7 13 L11 17 M8.75 15.25 L6.25 17.75"
          stroke="#44403c"
          stroke-width="2.25"
        />
      </g>
    </svg>
    """
  end
end

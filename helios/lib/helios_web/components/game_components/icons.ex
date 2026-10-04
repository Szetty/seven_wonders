defmodule HeliosWeb.GameComponents.Icons do
  @moduledoc "Game symbols that heroicons has no match for."
  use HeliosWeb, :html

  attr :class, :any, default: "size-5"
  attr :rest, :global

  @doc "The red shield of military strength, as printed on military cards."
  def military_icon(assigns) do
    ~H"""
    <svg
      data-icon="military"
      viewBox="0 0 20 24"
      class={@class}
      aria-hidden="true"
      {@rest}
    >
      <path
        d="M10 1.5 L18.5 4.5 V11 C18.5 16.5 15 20.5 10 22.5 C5 20.5 1.5 16.5 1.5 11 V4.5 Z"
        fill="#b91c1c"
        stroke="#d4a017"
        stroke-width="2"
      />
      <path d="M10 6 V18 M6 11 H14" stroke="#fde68a" stroke-width="1.6" stroke-linecap="round" />
    </svg>
    """
  end
end

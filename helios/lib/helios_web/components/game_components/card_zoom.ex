defmodule HeliosWeb.GameComponents.CardZoom do
  @moduledoc "Full-size card art over a dark scrim. Client-side only (JS show/hide)."
  use HeliosWeb, :html

  alias HeliosWeb.GameAssets

  @doc "Opens the zoom rendered by `card_zoom/1`."
  def show_zoom do
    JS.show(to: "#card-zoom", display: "flex") |> JS.focus(to: "#close-card-zoom")
  end

  attr :name, :string, required: true

  def card_zoom(assigns) do
    ~H"""
    <div
      id="card-zoom"
      role="dialog"
      aria-modal="true"
      aria-label={@name}
      phx-click={JS.hide(to: "#card-zoom")}
      phx-window-keydown={JS.hide(to: "#card-zoom")}
      phx-key="Escape"
      class="fixed inset-0 z-[60] hidden items-center justify-center bg-black/80 p-4"
    >
      <img
        src={GameAssets.card_path(@name)}
        alt={@name}
        class="max-h-[min(440px,85dvh)] w-auto rounded-xl shadow-2xl"
      />
      <button
        id="close-card-zoom"
        type="button"
        aria-label="Close"
        class="absolute right-4 top-4 inline-flex size-10 items-center justify-center rounded-full bg-white/15 text-white transition hover:bg-white/30 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-6" />
      </button>
    </div>
    """
  end
end

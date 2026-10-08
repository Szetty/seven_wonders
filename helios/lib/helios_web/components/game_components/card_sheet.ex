defmodule HeliosWeb.GameComponents.CardSheet do
  @moduledoc "Read-only card details (any built card on any board)."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.CardInfo, only: [card_info: 1]
  import HeliosWeb.GameComponents.CardZoom, only: [card_zoom: 1, show_zoom: 0]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :name, :string, required: true
  attr :neighbours, :map, required: true
  attr :built, :list, required: true

  def card_sheet(assigns) do
    ~H"""
    <div
      id="card-sheet-scrim"
      phx-click="hide_card"
      class="fixed inset-0 z-40 bg-black/35"
      aria-hidden="true"
    >
    </div>
    <section
      id="card-sheet"
      role="dialog"
      aria-modal="true"
      aria-labelledby="card-sheet-title"
      phx-window-keydown="hide_card"
      phx-key="Escape"
      class={GameFormat.sheet_class()}
    >
      <button
        id="close-card-sheet"
        type="button"
        phx-click="hide_card"
        aria-label="Close"
        class="absolute right-2 top-2 inline-flex size-9 items-center justify-center rounded-full text-zinc-500 transition hover:bg-zinc-100 hover:text-zinc-900 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-5" />
      </button>
      <div class="flex items-start gap-3 sm:gap-4">
        <button
          id="zoom-card"
          type="button"
          phx-click={show_zoom()}
          aria-label={"Enlarge #{@name}"}
          class="shrink-0 cursor-zoom-in rounded-lg"
        >
          <img
            src={GameAssets.card_path(@name)}
            alt={@name}
            class="h-[147px] w-24 rounded-lg sm:h-[183px] sm:w-[120px]"
          />
        </button>
        <div class="flex min-w-0 flex-1 flex-col gap-2">
          <h3 id="card-sheet-title" class="pr-10 text-lg font-semibold text-zinc-900">{@name}</h3>
          <.card_info name={@name} neighbours={@neighbours} built={@built} />
        </div>
      </div>
      <.card_zoom name={@name} />
    </section>
    """
  end
end

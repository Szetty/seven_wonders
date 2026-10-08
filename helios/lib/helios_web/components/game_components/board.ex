defmodule HeliosWeb.GameComponents.Board do
  @moduledoc """
  The current player's own area: wonder board with stage slots, built cards in
  colour columns, and counters. The engine does not expose the age in which
  each stage was built, so slot n shows the age-n card back (capped at III).
  """
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.PlayerPanels, only: [player_stats: 1, status_chip: 1]
  import HeliosWeb.GameComponents.StageStrip, only: [stage_strip: 1]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :player, :map, required: true
  attr :status, :atom, default: nil

  def my_board(assigns) do
    assigns = assign(assigns, :columns, GameFormat.group_by_category(assigns.player.built))

    ~H"""
    <section
      id="my-board"
      data-player={@player.name}
      class="min-w-0 order-first flex flex-col gap-3 rounded-xl bg-antique/90 p-2 shadow-lg sm:p-3 md:col-span-2 lg:order-none lg:col-span-1"
    >
      <div class="flex items-center justify-between gap-2">
        <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">You</span>
        <.status_chip id={"status-#{@player.name}"} status={@status} />
      </div>
      <div class="relative overflow-hidden rounded-lg">
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
          class="aspect-[16/5] w-full object-cover"
        />
        <.stage_strip id="wonder-stages" player={@player} owner="You" />
      </div>
      <.player_stats player={@player} />
      <div id="my-built" class="flex gap-2 overflow-x-auto pb-1">
        <div
          :for={{category, cards} <- @columns}
          data-category={category}
          class="flex shrink-0 flex-col gap-1"
        >
          <div
            :for={card <- cards}
            id={"built-#{GameAssets.slug(card.name)}"}
            data-card={card.name}
            title={card.name}
            class={["h-10 w-16 overflow-hidden rounded ring-2", GameFormat.category_ring(category)]}
          >
            <img
              src={GameAssets.card_path(card.name)}
              alt={card.name}
              class="w-full object-cover object-top"
            />
          </div>
        </div>
      </div>
    </section>
    """
  end
end

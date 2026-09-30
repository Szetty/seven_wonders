defmodule HeliosWeb.GameComponents.Board do
  @moduledoc """
  The current player's own area: wonder board with stage slots, built cards in
  colour columns, and counters. The engine does not expose the age in which
  each stage was built, so slot n shows the age-n card back (capped at III).
  """
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.PlayerPanels, only: [player_stats: 1]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :player, :map, required: true

  def my_board(assigns) do
    assigns = assign(assigns, :columns, GameFormat.group_by_category(assigns.player.built))

    ~H"""
    <section
      id="my-board"
      data-player={@player.name}
      class="min-w-0 order-first flex flex-col gap-3 rounded-xl bg-antique/90 p-3 shadow-lg lg:order-none"
    >
      <div class="relative overflow-hidden rounded-lg">
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
          class="aspect-[16/5] w-full object-cover"
        />
        <div
          id="wonder-stages"
          class="absolute inset-x-0 bottom-0 flex justify-center gap-2 bg-linear-to-t from-black/50 to-transparent p-2"
        >
          <div
            :for={stage <- 1..@player.stages_total//1}
            id={"stage-#{stage}"}
            data-built={to_string(stage <= @player.stages_built)}
            class={[
              "h-12 w-8 overflow-hidden rounded ring-2 transition sm:h-16 sm:w-11",
              if(stage <= @player.stages_built,
                do: "ring-amber-400",
                else: "bg-black/30 ring-white/60"
              )
            ]}
          >
            <img
              :if={stage <= @player.stages_built}
              src={GameAssets.card_back_path(min(stage, 3))}
              alt={"Stage #{stage} built"}
              class="h-full w-full object-cover"
            />
          </div>
        </div>
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

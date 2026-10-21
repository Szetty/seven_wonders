defmodule HeliosWeb.GameComponents.PlayerBoard do
  @moduledoc """
  One player's city: wonder with its stage strip, stats, and every built card
  as a tile (the latest one glows gold). Renders your board — always full — and
  everyone else's as a fixed-height compact summary (header, neighbour line,
  wonder, stats, category chips) whose cards open from `#<id>-toggle` into
  `#<id>-details` (a floating panel on `xl`, expanded in place below).
  """
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.PlayerPanels, only: [player_stats: 1, status_chip: 1]
  import HeliosWeb.GameComponents.StageStrip, only: [stage_strip: 1]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :id, :string, required: true
  attr :player, :map, required: true
  attr :names, :map, required: true
  attr :label, :string, required: true
  attr :status, :atom, default: nil
  attr :mine, :boolean, default: false
  attr :ai, :boolean, default: false, doc: "an AI seat (bot icon)"
  attr :random, :boolean, default: false, doc: "this player's latest play was a random fallback"
  attr :class, :any, default: nil
  attr :neighbours, :map, default: nil, doc: "resolved neighbour names + arrows; omit to hide"
  attr :panel_align, :atom, default: :left, doc: "which edge the cards panel hangs from on xl"
  attr :style, :any, default: nil, doc: "inline style (ring CSS custom properties on xl)"

  def player_board(assigns) do
    player = assigns.player
    name = if assigns.mine, do: "You", else: GameFormat.player_name(assigns.names, player.name)

    assigns =
      assign(assigns,
        name: name,
        columns: GameFormat.group_by_category(player.built),
        latest: List.last(player.built),
        chips: GameFormat.category_counts(player.built),
        strip_id: if(assigns.mine, do: "wonder-stages", else: "wonder-stages-#{player.name}"),
        built_id: if(assigns.mine, do: "my-built", else: "built-cards-#{player.name}")
      )

    ~H"""
    <section
      id={@id}
      data-player={@player.name}
      style={@style}
      class={[
        "group relative min-w-0 flex flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow sm:p-3",
        @class
      ]}
    >
      <span
        :if={@random}
        id={"random-badge-#{@player.name}"}
        data-random-badge
        class={[
          "rounded bg-amber-100 px-1.5 py-0.5 text-xs font-semibold text-amber-800",
          if(@mine, do: "self-start", else: "absolute top-2 right-2 z-10")
        ]}
      >
        random move
      </span>
      <div
        id={"#{@id}-summary"}
        class={[
          "flex min-w-0 flex-col gap-2",
          !@mine && "h-60 justify-between overflow-hidden"
        ]}
      >
        <%= if @mine do %>
          <header class="flex items-center justify-between gap-2">
            <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
            <.status_chip id={"status-#{@player.name}"} status={@status} />
            <span :if={@ai} data-ai-icon title="AI player" class="shrink-0 text-sky-700">
              <.icon name="hero-cpu-chip" class="size-4" />
            </span>
          </header>
        <% else %>
          <header class="flex items-center gap-2">
            <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
            <span class="min-w-0 truncate font-semibold text-zinc-900">{@name}</span>
            <span :if={@ai} data-ai-icon title="AI player" class="shrink-0 text-sky-700">
              <.icon name="hero-cpu-chip" class="size-4" />
            </span>
            <span class="ml-auto shrink-0">
              <.status_chip id={"status-#{@player.name}"} status={@status} />
            </span>
          </header>
        <% end %>
        <p
          :if={@neighbours}
          data-neighbours
          aria-label={"Neighbours: #{@neighbours.west} and #{@neighbours.east}"}
          class="flex items-center gap-1 text-xs text-zinc-600"
        >
          <span class="inline-flex items-center gap-0.5">
            <span data-ring-arrow="west" aria-hidden="true" class="hidden xl:inline">
              {elem(@neighbours.ring, 0)}
            </span>
            <span data-list-arrow="west" aria-hidden="true" class="xl:hidden">
              {elem(@neighbours.list, 0)}
            </span>
            {@neighbours.west}
          </span>
          <span aria-hidden="true">·</span>
          <span class="inline-flex items-center gap-0.5">
            <span data-ring-arrow="east" aria-hidden="true" class="hidden xl:inline">
              {elem(@neighbours.ring, 1)}
            </span>
            <span data-list-arrow="east" aria-hidden="true" class="xl:hidden">
              {elem(@neighbours.list, 1)}
            </span>
            {@neighbours.east}
          </span>
        </p>
        <div class="relative overflow-hidden rounded-lg">
          <img
            src={GameAssets.wonder_path(@player.wonder, @player.side)}
            alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
            class={["w-full object-cover", @mine && "aspect-[16/5]", !@mine && "h-20"]}
          />
          <.stage_strip id={@strip_id} player={@player} owner={@name} />
        </div>
        <.player_stats
          player={@player}
          class={if(@mine, do: "flex-wrap", else: "flex-nowrap overflow-hidden")}
        />
        <button
          :if={!@mine}
          id={"#{@id}-toggle"}
          type="button"
          data-board-toggle
          aria-controls={"#{@id}-details"}
          aria-expanded="false"
          class="flex w-full items-center gap-2 text-left transition active:opacity-70 pointer-coarse:min-h-11"
        >
          <span class="flex min-w-0 flex-1 items-center gap-1 overflow-hidden">
            <span
              :for={{category, count} <- @chips}
              data-chip={category}
              title={GameFormat.category_label(category)}
              class={[
                "flex shrink-0 items-center gap-0.5 rounded-full px-1.5 py-0.5 text-xs font-semibold text-white",
                GameFormat.category_class(category)
              ]}
            >
              {count}
            </span>
          </span>
          <.icon
            name="hero-chevron-down"
            class="size-4 shrink-0 text-zinc-500 transition group-[.is-open]:rotate-180"
          />
        </button>
      </div>
      <div
        id={"#{@id}-details"}
        data-panel-align={if(@mine, do: nil, else: @panel_align)}
        class={[
          "flex-col gap-2",
          @mine && "flex",
          !@mine &&
            "hidden group-[.is-open]:block xl:absolute xl:top-full xl:mt-2 xl:z-40 xl:w-max xl:max-w-[min(40rem,calc(100vw-2rem))] xl:shadow-xl",
          !@mine && @panel_align == :right && "xl:right-0",
          !@mine && @panel_align == :left && "xl:left-0"
        ]}
      >
        <div
          id={@built_id}
          class={[
            "flex gap-2 overflow-x-auto pb-1",
            @mine && "xl:max-h-[var(--me-built-max)] xl:overflow-y-auto"
          ]}
        >
          <div
            :for={{category, cards} <- @columns}
            data-category={category}
            class="flex shrink-0 flex-col gap-1"
          >
            <button
              :for={card <- cards}
              id={"built-#{@player.name}-#{GameAssets.slug(card.name)}"}
              type="button"
              data-card={card.name}
              data-latest={card == @latest}
              title={card.name}
              phx-click="show_card"
              phx-value-card={card.name}
              phx-value-owner={@player.name}
              class={[
                "h-10 w-16 overflow-hidden rounded ring-2 transition hover:-translate-y-0.5 pointer-coarse:min-h-11",
                if(card == @latest,
                  do: "ring-amber-400 shadow-[0_0_10px_rgba(245,158,11,0.9)]",
                  else: GameFormat.category_ring(category)
                )
              ]}
            >
              <img
                src={GameAssets.card_path(card.name)}
                alt={card.name}
                class="w-full object-cover object-top"
              />
            </button>
          </div>
        </div>
      </div>
    </section>
    """
  end
end

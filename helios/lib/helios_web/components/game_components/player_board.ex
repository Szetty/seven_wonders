defmodule HeliosWeb.GameComponents.PlayerBoard do
  @moduledoc """
  One player's city: wonder with its stage strip, stats, and every built card
  as a tile (the latest one glows gold). Renders your board, your neighbours'
  and everyone else's; boards other than yours fold into a one-line summary
  below `md` (toggle via `JS.toggle_class("is-open")`, no server state).
  """
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.Icons, only: [military_icon: 1]
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

  def player_board(assigns) do
    player = assigns.player
    name = if assigns.mine, do: "You", else: GameFormat.player_name(assigns.names, player.name)

    assigns =
      assign(assigns,
        name: name,
        columns: GameFormat.group_by_category(player.built),
        latest: List.last(player.built),
        strip_id: if(assigns.mine, do: "wonder-stages", else: "wonder-stages-#{player.name}"),
        built_id: if(assigns.mine, do: "my-built", else: "built-cards-#{player.name}")
      )

    ~H"""
    <section
      id={@id}
      data-player={@player.name}
      class={["group min-w-0 flex flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow sm:p-3", @class]}
    >
      <span
        :if={@random}
        id={"random-badge-#{@player.name}"}
        data-random-badge
        class="self-start rounded bg-amber-100 px-1.5 py-0.5 text-xs font-semibold text-amber-800"
      >
        random move
      </span>
      <button
        :if={!@mine}
        id={"#{@id}-toggle"}
        type="button"
        aria-expanded="false"
        aria-controls={"#{@id}-details"}
        phx-click={
          JS.toggle_class("is-open", to: "##{@id}")
          |> JS.toggle_attribute({"aria-expanded", "true", "false"})
        }
        class="flex w-full items-center gap-2 text-left transition active:opacity-70 md:hidden pointer-coarse:min-h-11"
      >
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt=""
          class="aspect-[16/5] w-18 shrink-0 rounded object-cover"
        />
        <span class="flex min-w-0 flex-1 flex-col">
          <span class="flex items-baseline gap-1.5">
            <span
              :if={@label != ""}
              class="text-xs font-semibold uppercase tracking-wide text-zinc-500"
            >
              {@label}
            </span>
            <span class="truncate font-semibold text-zinc-900">{@name}</span>
            <span :if={@ai} data-ai-icon title="AI player" class="shrink-0 text-sky-700">
              <.icon name="hero-cpu-chip" class="size-4" />
            </span>
            <span
              :if={@status}
              data-status-dot={@status}
              class={[
                "size-2 shrink-0 rounded-full",
                if(@status == :chosen, do: "bg-emerald-500", else: "bg-sky-400")
              ]}
            >
            </span>
          </span>
          <span class="flex items-center gap-2 text-xs text-zinc-700">
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:coin)} alt="" class="size-3.5" />{@player.coins}
            </span>
            <span class="flex items-center gap-0.5" title="Military strength">
              <.military_icon class="size-3.5" />{@player.shields}
            </span>
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:pyramid)} alt="" class="size-3.5" />{@player.stages_built}/{@player.stages_total}
            </span>
            <span class="flex min-w-0 flex-wrap gap-0.5">
              <span
                :for={card <- @player.built}
                class={["size-2.5 rounded-sm", GameFormat.category_class(card.category)]}
              >
              </span>
            </span>
          </span>
        </span>
        <.icon
          name="hero-chevron-down"
          class="size-4 shrink-0 text-zinc-500 transition group-[.is-open]:rotate-180"
        />
      </button>
      <div
        id={"#{@id}-details"}
        class={["flex-col gap-2", if(@mine, do: "flex", else: "hidden group-[.is-open]:flex md:flex")]}
      >
        <header class="flex items-center justify-between gap-2">
          <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
          <.status_chip id={"status-#{@player.name}"} status={@status} />
          <span :if={!@mine} class="min-w-0 truncate font-semibold text-zinc-900">{@name}</span>
          <span :if={@ai} data-ai-icon title="AI player" class="shrink-0 text-sky-700">
            <.icon name="hero-cpu-chip" class="size-4" />
          </span>
        </header>
        <div class="relative overflow-hidden rounded-lg">
          <img
            src={GameAssets.wonder_path(@player.wonder, @player.side)}
            alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
            class="aspect-[16/5] w-full object-cover"
          />
          <.stage_strip id={@strip_id} player={@player} owner={@name} />
        </div>
        <.player_stats player={@player} />
        <div id={@built_id} class="flex gap-2 overflow-x-auto pb-1">
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

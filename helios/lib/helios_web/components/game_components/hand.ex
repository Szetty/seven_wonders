defmodule HeliosWeb.GameComponents.Hand do
  @moduledoc "The player's hand, the action panel for the selected card, and the pending-choice banner."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.CardInfo, only: [card_info: 1]
  import HeliosWeb.GameComponents.CardZoom, only: [card_zoom: 1, show_zoom: 0]

  alias Helios.Games
  alias HeliosWeb.{GameAssets, GameFormat}

  @row "flex w-full items-center justify-between gap-3 rounded-lg bg-zinc-900 px-3 py-2 text-left text-sm font-semibold text-white shadow transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11"
  @row_disabled "flex w-full cursor-not-allowed items-center justify-between gap-3 rounded-lg bg-zinc-200 px-3 py-2 text-left text-sm text-zinc-500 pointer-coarse:min-h-11"
  @row_discard "flex w-full items-center justify-between gap-3 rounded-lg bg-white px-3 py-2 text-left text-sm font-semibold text-zinc-900 shadow transition ring-1 ring-zinc-300 hover:bg-zinc-100 active:bg-zinc-200 pointer-coarse:min-h-11"
  @payment "flex w-full items-center justify-between gap-3 rounded-lg bg-sky-50 px-3 py-2 text-left text-sm font-semibold text-sky-900 ring-1 ring-sky-200 transition hover:bg-sky-100 active:bg-sky-200 pointer-coarse:min-h-11"

  attr :hand, :list, required: true
  attr :selected, :string, default: nil
  attr :pending, :any, default: nil

  def hand(assigns) do
    assigns = assign(assigns, :pending_card, GameFormat.action_card(assigns.pending))

    ~H"""
    <section id="hand" phx-hook=".HandRects" aria-label="Your hand">
      <h2 class="sr-only text-xs font-semibold uppercase tracking-wide text-zinc-700 sm:not-sr-only short:sr-only">
        Your hand
      </h2>
      <div class="-mx-2 flex snap-x justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1 sm:pt-3">
        <button
          :for={{card, index} <- Enum.with_index(@hand)}
          type="button"
          id={"hand-card-#{index}"}
          phx-click="select_card"
          phx-value-card={card.name}
          data-card={card.name}
          data-buildable={to_string(GameFormat.available?(card.build))}
          aria-pressed={to_string(@selected == card.name)}
          class={[
            "relative shrink-0 snap-start rounded-lg transition duration-150 hover:-translate-y-1 active:scale-95 focus:outline-none focus-visible:ring-4 focus-visible:ring-sky-300",
            @selected == card.name && "-translate-y-2 ring-4 ring-sky-500",
            @pending_card == card.name && "ring-4 ring-amber-500"
          ]}
        >
          <img
            src={GameAssets.card_path(card.name)}
            alt={card.name}
            class={[
              "h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 lg:h-[183px] lg:w-[120px] short:h-[86px] short:w-14",
              !GameFormat.available?(card.build) && "opacity-70 grayscale"
            ]}
          />
          <.cost_badge option={card.build} />
          <span
            :if={@pending_card == card.name}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </button>
      </div>
    </section>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".HandRects">
      export default {
        mounted() { this.snapshot() },
        beforeUpdate() { this.snapshot() },
        snapshot() {
          const rects = {}
          this.el.querySelectorAll("[data-card] > img").forEach((img) => {
            const r = img.getBoundingClientRect()
            rects[img.closest("[data-card]").dataset.card] =
              {left: r.left, top: r.top, width: r.width, height: r.height, src: img.currentSrc || img.src}
          })
          window.sevenWondersHandRects = rects
        },
      }
    </script>
    """
  end

  attr :hand, :list, required: true, doc: "card names"
  attr :chosen, :string, default: nil

  def replay_hand(assigns) do
    ~H"""
    <section id="replay-hand" phx-hook=".HandRects" aria-label="Your hand that turn">
      <div class="-mx-2 flex justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1">
        <div
          :for={card <- @hand}
          data-card={card}
          class={["relative shrink-0 rounded-lg", card == @chosen && "ring-4 ring-amber-500"]}
        >
          <img
            src={GameAssets.card_path(card)}
            alt={card}
            class="h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 short:h-[86px] short:w-14"
          />
          <span
            :if={card == @chosen}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </div>
      </div>
    </section>
    """
  end

  attr :option, :any, required: true

  defp cost_badge(assigns) do
    assigns = assign(assigns, :cheapest, GameFormat.cheapest(assigns.option))

    ~H"""
    <span
      class={[
        "cost-badge pointer-events-none absolute -right-1.5 -top-1.5 flex h-6 min-w-6 items-center justify-center gap-0.5 rounded-full px-1 text-xs font-extrabold shadow ring-1",
        badge_class(@cheapest)
      ]}
      data-cost={badge_data(@cheapest)}
      title={GameFormat.cheapest_label(@option)}
    >
      <%= case @cheapest do %>
        <% {:coins, n} -> %>
          {n}<img src={GameAssets.token_path(:coin)} alt="" class="size-3.5" />
        <% {:unavailable, _} -> %>
          <.icon name="hero-x-mark" class="size-4" />
        <% _free -> %>
          <img src={GameAssets.token_path(:check)} alt="" class="size-4" />
      <% end %>
    </span>
    """
  end

  defp badge_data({:coins, n}), do: to_string(n)
  defp badge_data({:unavailable, _}), do: "unavailable"
  defp badge_data(_free), do: "free"

  defp badge_class({:coins, _}), do: "bg-amber-50 text-zinc-900 ring-amber-500"
  defp badge_class({:unavailable, _}), do: "bg-zinc-500 text-white ring-zinc-700"
  defp badge_class(_free), do: "bg-emerald-600 text-white ring-emerald-800"

  attr :card, :map, required: true
  attr :neighbours, :map, required: true
  attr :built, :list, default: [], doc: "names of the viewer's built cards (for the ✓ on chains)"
  attr :next_stage, :integer, default: nil

  def action_panel(assigns) do
    assigns = assign(assigns, row_class: @row, discard_class: @row_discard)

    ~H"""
    <div
      id="action-scrim"
      phx-click="deselect"
      class="fixed inset-0 z-40 bg-black/35"
      aria-hidden="true"
    >
    </div>
    <section
      id="action-panel"
      role="dialog"
      aria-modal="true"
      aria-labelledby="action-panel-title"
      phx-window-keydown="deselect"
      phx-key="Escape"
      class={GameFormat.sheet_class()}
    >
      <div class="mx-auto mb-3 h-1 w-10 rounded-full bg-zinc-300 lg:hidden" aria-hidden="true"></div>
      <button
        id="close-action-panel"
        type="button"
        phx-click="deselect"
        aria-label="Close"
        class="absolute right-2 top-2 inline-flex size-9 items-center justify-center rounded-full text-zinc-500 transition hover:bg-zinc-100 hover:text-zinc-900 active:bg-zinc-200 active:text-zinc-900 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-5" />
      </button>
      <div class="flex items-start gap-3 sm:gap-4">
        <button
          id="zoom-card"
          type="button"
          phx-click={show_zoom()}
          aria-label={"Enlarge #{@card.name}"}
          class="relative shrink-0 cursor-zoom-in rounded-lg focus:outline-none focus-visible:ring-4 focus-visible:ring-sky-300"
        >
          <img
            src={GameAssets.card_path(@card.name)}
            alt={@card.name}
            class="h-[147px] w-24 rounded-lg sm:h-[183px] sm:w-[120px] short:h-[98px] short:w-16"
          />
          <span class="absolute bottom-1 right-1 rounded-full bg-black/60 p-1 text-white">
            <.icon name="hero-magnifying-glass-plus" class="size-4" />
          </span>
        </button>
        <div class="flex min-w-0 flex-1 flex-col gap-2">
          <h3 id="action-panel-title" class="pr-10 text-lg font-semibold text-zinc-900">
            {@card.name}
          </h3>
          <.card_info name={@card.name} neighbours={@neighbours} built={@built} />
          <.action_row
            id="build"
            kind="build"
            title="Build"
            card={@card.name}
            option={@card.build}
            neighbours={@neighbours}
          />
          <.action_row
            id="wonder"
            kind="wonder_stage"
            title={
              if @next_stage, do: "Build wonder stage #{@next_stage}", else: "Build wonder stage"
            }
            card={@card.name}
            option={@card.wonder_stage}
            neighbours={@neighbours}
          />
          <button
            :if={@card.free_build}
            id="build-free-button"
            type="button"
            phx-click="submit"
            phx-value-card={@card.name}
            phx-value-kind="build_free"
            phx-value-option="0"
            class={@row_class}
          >
            <span>Build for free (Olympía)</span><span class="font-bold">Free</span>
          </button>
          <button
            id="discard-button"
            type="button"
            phx-click="submit"
            phx-value-card={@card.name}
            phx-value-kind="discard"
            phx-value-option="0"
            class={@discard_class}
          >
            <span class="flex items-center gap-1">
              <.icon name="hero-trash" class="size-4" /> Discard
            </span>
            <span class="flex items-center gap-1 font-bold">
              +3 <img src={GameAssets.token_path(:coin)} alt="coins" class="size-4" />
            </span>
          </button>
        </div>
      </div>
      <.card_zoom name={@card.name} />
    </section>
    """
  end

  attr :id, :string, required: true
  attr :kind, :string, required: true
  attr :title, :string, required: true
  attr :card, :string, required: true
  attr :option, :any, required: true
  attr :neighbours, :map, required: true

  defp action_row(assigns) do
    assigns = assign(assigns, row: @row, row_disabled: @row_disabled, payment: @payment)

    ~H"""
    <%= case @option do %>
      <% {:unavailable, reason} -> %>
        <button id={"#{@id}-action"} type="button" disabled class={@row_disabled}>
          <span>{@title}</span><span>{Games.error_message(reason)}</span>
        </button>
      <% {:trade, [_, _ | _] = options} -> %>
        <button
          id={"#{@id}-action"}
          type="button"
          aria-expanded="false"
          aria-controls={"#{@id}-payments"}
          phx-click={
            JS.toggle(to: "##{@id}-payments", display: "flex")
            |> JS.toggle_attribute({"aria-expanded", "true", "false"})
          }
          class={@row}
        >
          <span>{@title}</span>
          <span class="flex items-center gap-1 font-bold">
            from {GameFormat.cheapest_label(@option)}
            <.icon name="hero-chevron-down" class="size-4" />
          </span>
        </button>
        <div
          id={"#{@id}-payments"}
          class="ml-2 hidden flex-col gap-1.5 border-l-4 border-sky-400 pl-2"
        >
          <p :if={GameFormat.card_cost(@option)} id={"#{@id}-card-cost"} class="text-xs text-zinc-600">
            includes {GameFormat.card_cost(@option)} {if GameFormat.card_cost(@option) == 1,
              do: "coin",
              else: "coins"} card cost
          </p>
          <button
            :for={
              {payment_option, index} <-
                options
                |> Enum.with_index()
                |> Enum.sort_by(fn {o, i} -> {GameFormat.option_total(o), i} end)
            }
            id={"#{@id}-option-#{index}"}
            type="button"
            phx-click="submit"
            phx-value-card={@card}
            phx-value-kind={@kind}
            phx-value-option={index}
            class={@payment}
          >
            <span>{GameFormat.payment_label(payment_option, @neighbours)}</span>
            <span class="shrink-0 font-bold">{GameFormat.option_total(payment_option)} coins</span>
          </button>
        </div>
      <% single -> %>
        <button
          id={"#{@id}-action"}
          type="button"
          phx-click="submit"
          phx-value-card={@card}
          phx-value-kind={@kind}
          phx-value-option="0"
          class={@row}
        >
          <span>{@title}</span><span class="font-bold">{GameFormat.cheapest_label(single)}</span>
        </button>
    <% end %>
    """
  end

  attr :action, :any, required: true

  def pending_choice(assigns) do
    ~H"""
    <div
      id="pending-choice"
      class="flex flex-wrap items-center justify-between gap-2 rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300"
    >
      <span class="flex items-center gap-2 text-zinc-800">
        <.icon name="hero-check-circle" class="size-5 text-amber-600" /> You chose:
        <strong>{GameFormat.describe_action(@action)}</strong>
      </span>
      <button
        id="change-choice"
        type="button"
        phx-click="select_card"
        phx-value-card={GameFormat.action_card(@action)}
        class="inline-flex items-center rounded-lg bg-white px-3 py-1.5 text-sm font-semibold text-zinc-900 ring-1 ring-amber-300 transition hover:bg-amber-100 active:bg-amber-200 pointer-coarse:min-h-11"
      >
        Change
      </button>
    </div>
    """
  end
end

defmodule HeliosWeb.GameComponents.Hand do
  @moduledoc "The player's hand, the action panel for the selected card, and the pending-choice banner."
  use HeliosWeb, :html

  alias Helios.Games
  alias HeliosWeb.{GameAssets, GameFormat}

  @action_button "rounded-lg bg-zinc-900 px-3 py-2 text-sm font-semibold text-white shadow transition hover:-translate-y-0.5 hover:bg-zinc-700"
  @disabled_button "cursor-not-allowed rounded-lg bg-zinc-200 px-3 py-2 text-sm text-zinc-500"

  attr :hand, :list, required: true
  attr :selected, :string, default: nil
  attr :pending, :any, default: nil

  def hand(assigns) do
    assigns = assign(assigns, :pending_card, GameFormat.action_card(assigns.pending))

    ~H"""
    <section id="hand" class="rounded-xl bg-antique/90 p-3 shadow-lg">
      <h2 class="mb-2 text-sm font-semibold uppercase tracking-wide text-zinc-700">Your hand</h2>
      <div class="flex gap-2 overflow-x-auto px-1 pb-2 pt-3">
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
            "relative shrink-0 rounded-lg transition duration-150 hover:-translate-y-1 focus:outline-none focus-visible:ring-4 focus-visible:ring-sky-300",
            @selected == card.name && "-translate-y-2 ring-4 ring-sky-500",
            @pending_card == card.name && "ring-4 ring-amber-500"
          ]}
        >
          <img
            src={GameAssets.card_path(card.name)}
            alt={card.name}
            class="h-[183px] w-[120px] rounded-lg object-cover sm:h-[275px] sm:w-[180px]"
          />
          <span
            :if={@pending_card == card.name}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </button>
      </div>
    </section>
    """
  end

  attr :card, :map, required: true

  def action_panel(assigns) do
    assigns = assign(assigns, :button_class, @action_button)

    ~H"""
    <section id="action-panel" class="rounded-xl bg-white/95 p-4 shadow-xl ring-1 ring-black/5">
      <div class="flex items-start gap-4">
        <img
          src={GameAssets.card_path(@card.name)}
          alt={@card.name}
          class="hidden h-[183px] w-[120px] rounded-lg sm:block"
        />
        <div class="flex flex-1 flex-col gap-3">
          <h3 class="text-lg font-semibold text-zinc-900">{@card.name}</h3>
          <.option_group id="build-options" title="Build" kind="build" prefix="build" card={@card.name} option={@card.build} />
          <.option_group
            id="wonder-options"
            title="Build wonder stage"
            kind="wonder_stage"
            prefix="wonder"
            card={@card.name}
            option={@card.wonder_stage}
          />
          <div :if={@card.free_build}>
            <button
              id="build-free-button"
              type="button"
              phx-click="submit"
              phx-value-card={@card.name}
              phx-value-kind="build_free"
              phx-value-option="0"
              class={@button_class}
            >
              Build for free (Olympía)
            </button>
          </div>
          <div>
            <button
              id="discard-button"
              type="button"
              phx-click="submit"
              phx-value-card={@card.name}
              phx-value-kind="discard"
              phx-value-option="0"
              class="rounded-lg bg-white px-3 py-2 text-sm font-semibold text-zinc-900 ring-1 ring-zinc-300 transition hover:bg-zinc-100"
            >
              <.icon name="hero-trash" class="size-4" /> Discard (+3 coins)
            </button>
          </div>
        </div>
      </div>
    </section>
    """
  end

  attr :id, :string, required: true
  attr :title, :string, required: true
  attr :kind, :string, required: true
  attr :prefix, :string, required: true
  attr :card, :string, required: true
  attr :option, :any, required: true

  defp option_group(assigns) do
    assigns = assign(assigns, button_class: @action_button, disabled_class: @disabled_button)

    ~H"""
    <div id={@id} class="flex flex-col gap-1">
      <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@title}</span>
      <%= case @option do %>
        <% {:unavailable, reason} -> %>
          <button id={"#{@prefix}-unavailable"} type="button" disabled class={@disabled_class}>
            {Games.error_message(reason)}
          </button>
        <% {:trade, options} -> %>
          <div class="flex flex-wrap gap-2">
            <button
              :for={{payment_option, index} <- Enum.with_index(options)}
              id={"#{@prefix}-option-#{index}"}
              type="button"
              phx-click="submit"
              phx-value-card={@card}
              phx-value-kind={@kind}
              phx-value-option={index}
              class={@button_class}
            >
              {GameFormat.option_label(payment_option)}
            </button>
          </div>
        <% simple -> %>
          <div>
            <button
              id={"#{@prefix}-option-0"}
              type="button"
              phx-click="submit"
              phx-value-card={@card}
              phx-value-kind={@kind}
              phx-value-option="0"
              class={@button_class}
            >
              {GameFormat.option_label(simple)}
            </button>
          </div>
      <% end %>
    </div>
    """
  end

  attr :action, :any, required: true

  def pending_choice(assigns) do
    ~H"""
    <div
      id="pending-choice"
      class="flex items-center justify-between gap-3 rounded-xl bg-amber-50 px-4 py-3 ring-1 ring-amber-300"
    >
      <span class="flex items-center gap-2 text-zinc-800">
        <.icon name="hero-check-circle" class="size-5 text-amber-600" />
        You chose: <strong>{GameFormat.describe_action(@action)}</strong>
      </span>
      <button
        id="change-choice"
        type="button"
        phx-click="select_card"
        phx-value-card={GameFormat.action_card(@action)}
        class="rounded-lg bg-white px-3 py-1.5 text-sm font-semibold text-zinc-900 ring-1 ring-amber-300 transition hover:bg-amber-100"
      >
        Change
      </button>
    </div>
    """
  end
end

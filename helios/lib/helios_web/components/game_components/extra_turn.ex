defmodule HeliosWeb.GameComponents.ExtraTurn do
  @moduledoc "Extra-turn notices for the dock (Babylon B last card, waiting for others) and the Halikarnassós discard picker."
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :view, :map, required: true
  attr :names, :map, required: true

  def extra_turn_notice(%{view: %{phase: %{kind: :extra_turn}}} = assigns) do
    ~H"""
    <%= cond do %>
      <% @view.phase.extra_turn_player != @view.me -> %>
        <div
          id="waiting-extra-turn"
          class="flex items-center gap-2 rounded-lg bg-white/90 px-3 py-2 text-sm shadow-sm"
        >
          <.icon name="hero-clock" class="size-5 shrink-0 animate-pulse text-sky-600" />
          <span>
            Waiting for {GameFormat.player_name(@names, @view.phase.extra_turn_player)} ({GameFormat.extra_turn_label(
              @view.phase.extra_turn_kind
            )})
          </span>
        </div>
      <% @view.phase.extra_turn_kind == :play_last_card -> %>
        <div
          id="play-last-card"
          class="rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300"
        >
          <strong>Play your last card:</strong> build it, build a wonder stage with it, or discard it.
        </div>
      <% true -> %>
    <% end %>
    """
  end

  def extra_turn_notice(assigns), do: ~H""

  attr :view, :map, required: true

  def discard_picker(assigns) do
    ~H"""
    <%= if my_discard_turn?(@view) do %>
      <div id="discard-scrim" class="fixed inset-0 z-40 bg-black/35" aria-hidden="true"></div>
      <section
        id="discard-picker"
        role="dialog"
        aria-modal="true"
        aria-labelledby="discard-picker-title"
        class={GameFormat.sheet_class()}
      >
        <h2 id="discard-picker-title" class="mb-3 font-semibold text-zinc-900">
          Build one card from the discard pile for free
        </h2>
        <div class="grid grid-cols-3 gap-2 sm:flex sm:flex-wrap">
          <button
            :for={{card, index} <- Enum.with_index(@view.discard_pile || [])}
            id={"discard-pick-#{index}"}
            type="button"
            phx-click="submit"
            phx-value-card={card}
            phx-value-kind="build_from_discard"
            phx-value-option="0"
            data-card={card}
            class="rounded-lg transition hover:-translate-y-1 hover:ring-4 hover:ring-sky-400 active:scale-95 active:ring-4 active:ring-sky-400"
          >
            <img
              src={GameAssets.card_path(card)}
              alt={card}
              class="aspect-[120/183] w-full rounded-lg sm:h-[183px] sm:w-[120px]"
            />
          </button>
        </div>
      </section>
    <% end %>
    """
  end

  defp my_discard_turn?(%{
         me: me,
         phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard, extra_turn_player: me}
       }),
       do: true

  defp my_discard_turn?(_view), do: false
end

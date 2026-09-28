defmodule HeliosWeb.GameComponents.ExtraTurn do
  @moduledoc "Halikarnassós discard picker, Babylon B last-card notice, and the waiting notice for others."
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :view, :map, required: true
  attr :names, :map, required: true

  def extra_turn(%{view: %{phase: %{kind: :extra_turn}}} = assigns) do
    ~H"""
    <%= cond do %>
      <% @view.phase.extra_turn_player != @view.me -> %>
        <div
          id="waiting-extra-turn"
          class="flex items-center gap-2 rounded-xl bg-white/90 px-4 py-3 shadow"
        >
          <.icon name="hero-clock" class="size-5 animate-pulse text-sky-600" />
          Waiting for {GameFormat.player_name(@names, @view.phase.extra_turn_player)} ({GameFormat.extra_turn_label(
            @view.phase.extra_turn_kind
          )})
        </div>
      <% @view.phase.extra_turn_kind == :build_from_discard -> %>
        <section id="discard-picker" class="rounded-xl bg-antique/95 p-4 shadow-xl">
          <h2 class="mb-3 font-semibold text-zinc-900">
            Build one card from the discard pile for free
          </h2>
          <div class="flex flex-wrap gap-2">
            <button
              :for={{card, index} <- Enum.with_index(@view.discard_pile || [])}
              id={"discard-pick-#{index}"}
              type="button"
              phx-click="submit"
              phx-value-card={card}
              phx-value-kind="build_from_discard"
              phx-value-option="0"
              data-card={card}
              class="rounded-lg transition hover:-translate-y-1 hover:ring-4 hover:ring-sky-400"
            >
              <img src={GameAssets.card_path(card)} alt={card} class="h-[183px] w-[120px] rounded-lg" />
            </button>
          </div>
        </section>
      <% true -> %>
        <div id="play-last-card" class="rounded-xl bg-amber-50 px-4 py-3 ring-1 ring-amber-300">
          <strong>Play your last card:</strong> build it, build a wonder stage with it, or discard it.
        </div>
    <% end %>
    """
  end

  def extra_turn(assigns), do: ~H""
end

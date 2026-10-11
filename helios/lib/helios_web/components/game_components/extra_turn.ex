defmodule HeliosWeb.GameComponents.ExtraTurn do
  @moduledoc "Extra-turn notices for the dock (Babylon B last card, waiting for others), the Halikarnassós discard picker and its read-only replay counterpart."
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
  attr :minimised, :boolean, default: false

  def discard_picker(assigns) do
    ~H"""
    <%= if my_discard_turn?(@view) and not @minimised do %>
      <div id="discard-scrim" class="fixed inset-0 z-40 bg-black/35" aria-hidden="true"></div>
      <section
        id="discard-picker"
        phx-hook=".DiscardRects"
        role="dialog"
        aria-modal="true"
        aria-labelledby="discard-picker-title"
        class={GameFormat.sheet_class()}
      >
        <div class="mb-3 flex items-center justify-between gap-2">
          <h2 id="discard-picker-title" class="font-semibold text-zinc-900">
            Build one card from the discard pile for free
          </h2>
          <button
            id="discard-minimise"
            type="button"
            phx-click="minimise_discard"
            class="inline-flex shrink-0 items-center gap-1 rounded-lg px-2 py-1 text-sm font-semibold text-sky-700 transition hover:bg-sky-50 pointer-coarse:min-h-11"
          >
            <.icon name="hero-eye" class="size-4" /> Look at the boards
          </button>
        </div>
        <div class="grid grid-cols-3 gap-2 sm:flex sm:flex-wrap sm:justify-center">
          <button
            :for={{card, index} <- Enum.with_index(@view.discard_pile || [])}
            id={"discard-pick-#{index}"}
            type="button"
            phx-click="submit"
            phx-value-card={card}
            phx-value-kind="build_from_discard"
            phx-value-option="0"
            data-card={card}
            data-fx-id={"discard-card-#{GameAssets.slug(card)}"}
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
    <script :type={Phoenix.LiveView.ColocatedHook} name=".DiscardRects">
      // The picker / replay grid is gone by the time the turn resolves, so the
      // card rects are snapshotted here and read by .TurnFx (fx.ex). Pointer
      // and focus snapshots catch the final position after any sheet motion.
      export default {
        mounted() {
          this.snapshot = this.snapshot.bind(this)
          this.snapshot()
          this.el.addEventListener("pointerdown", this.snapshot, true)
          this.el.addEventListener("focusin", this.snapshot, true)
        },
        beforeUpdate() { this.snapshot() },
        snapshot() {
          const rects = {}
          this.el.querySelectorAll("[data-fx-id] img").forEach((img) => {
            const r = img.getBoundingClientRect()
            rects[img.closest("[data-fx-id]").dataset.fxId] =
              {left: r.left, top: r.top, width: r.width, height: r.height, src: img.currentSrc || img.src}
          })
          window.sevenWondersDiscardRects = rects
        },
      }
    </script>
    """
  end

  attr :view, :map, required: true
  attr :minimised, :boolean, required: true

  def discard_restore(assigns) do
    ~H"""
    <div
      :if={my_discard_turn?(@view) and @minimised}
      class="flex items-center justify-between gap-2 rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300"
    >
      <span>Pick a card from the discard pile</span>
      <button
        id="discard-restore"
        type="button"
        phx-click="restore_discard"
        class="inline-flex items-center rounded-lg bg-zinc-900 px-3 py-1.5 font-semibold text-white transition hover:bg-zinc-700 pointer-coarse:min-h-11"
      >
        Show
      </button>
    </div>
    """
  end

  attr :cards, :list, required: true, doc: "buildable discard names the viewer saw that turn"
  attr :chosen, :string, default: nil

  @doc "Read-only replay of the viewer's own build-from-discard choice."
  def replay_discard(assigns) do
    ~H"""
    <section
      id="replay-discard"
      phx-hook=".DiscardRects"
      aria-labelledby="replay-discard-title"
    >
      <h2
        id="replay-discard-title"
        class="px-1 text-xs font-semibold uppercase tracking-wide text-zinc-700"
      >
        Discard pile — you could build one for free
      </h2>
      <div class="-mx-2 flex justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1">
        <div
          :for={card <- @cards}
          id={"replay-discard-#{GameAssets.slug(card)}"}
          data-card={card}
          data-fx-id={"discard-card-#{GameAssets.slug(card)}"}
          data-chosen={to_string(card == @chosen)}
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

  def my_discard_turn?(%{
        me: me,
        phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard, extra_turn_player: me}
      }),
      do: true

  def my_discard_turn?(_view), do: false
end

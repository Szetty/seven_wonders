defmodule HeliosWeb.GameComponents.AgeSummary do
  @moduledoc "End-of-age military conflicts, revealed row by row; closes on click, Escape or after ~6 s."
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat, HistoryText}

  @close JS.dispatch("seven:close-summary", to: "#age-summary")

  def ordered(battles, me) do
    {mine, others} = Enum.split_with(battles, &(&1.player == me or &1.rival == me))
    mine ++ others
  end

  attr :event, :any, required: true
  attr :ctx, :map, required: true

  def age_summary(assigns) do
    {:battles, %{age: age, battles: battles}} = assigns.event
    assigns = assign(assigns, age: age, rows: ordered(battles, assigns.ctx.me), close: @close)

    ~H"""
    <div
      id="age-summary"
      phx-hook=".AgeSummary"
      phx-click={@close}
      phx-window-keydown={@close}
      phx-key="Escape"
      class="fixed inset-0 z-[55] flex items-center justify-center bg-black/55 p-4"
    >
      <section
        role="dialog"
        aria-modal="true"
        aria-labelledby="age-summary-title"
        class="w-full max-w-lg rounded-2xl bg-antique p-4 shadow-2xl sm:p-6"
      >
        <h2 id="age-summary-title" class="mb-3 text-center text-lg font-bold text-zinc-900">
          End of Age {GameFormat.roman(@age)} — Military conflicts
        </h2>
        <ul class="flex flex-col gap-2">
          <li
            :for={{battle, index} <- Enum.with_index(@rows)}
            id={"battle-#{index}"}
            class="grid grid-cols-[1fr_auto_1fr] items-center gap-2 rounded-lg bg-white/70 px-3 py-2 motion-safe:animate-[battle-in_400ms_ease-out_both]"
            style={"animation-delay: #{index * 350}ms"}
          >
            <span class="flex min-w-0 items-center justify-end gap-1 truncate text-right">
              {HistoryText.actor(battle.player, @ctx)} {battle.player_strength}
              <img
                :if={battle.player_token}
                data-token-for={battle.player}
                src={GameAssets.token_path({:military, battle.player_token})}
                alt={"#{battle.player_token}"}
                class="size-7"
              />
            </span>
            <span class="text-xs font-bold uppercase text-zinc-500">
              {if battle.player_token, do: "vs", else: "tie"}
            </span>
            <span class="flex min-w-0 items-center gap-1 truncate">
              <img
                :if={battle.rival_token}
                data-token-for={battle.rival}
                src={GameAssets.token_path({:military, battle.rival_token})}
                alt={"#{battle.rival_token}"}
                class="size-7"
              />
              {battle.rival_strength} {HistoryText.actor(battle.rival, @ctx)}
            </span>
          </li>
        </ul>
        <div class="mt-4 flex justify-center">
          <button
            id="close-age-summary"
            type="button"
            class="rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white transition hover:bg-zinc-700 pointer-coarse:min-h-11"
          >
            Continue
          </button>
        </div>
      </section>
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".AgeSummary">
      export default {
        mounted() {
          this.closed = false
          this.el.addEventListener("seven:close-summary", () => this.close())
          this.timer = setTimeout(() => this.close(), 6000)
        },
        destroyed() { clearTimeout(this.timer) },
        close() {
          if (this.closed) return
          this.closed = true
          const tokens = [...this.el.querySelectorAll("img[data-token-for]")].map((img) => {
            const r = img.getBoundingClientRect()
            return {player: img.dataset.tokenFor, src: img.src, rect: {left: r.left, top: r.top, width: r.width, height: r.height}}
          })
          window.dispatchEvent(new CustomEvent("fx:tokens", {detail: tokens}))
          this.pushEvent("dismiss_age_summary", {})
        },
      }
    </script>
    """
  end
end

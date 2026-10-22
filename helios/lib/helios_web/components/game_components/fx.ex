defmodule HeliosWeb.GameComponents.Fx do
  @moduledoc """
  Full-screen, click-through layer for turn animations. The `.TurnFx` hook
  flies the player's card from the hand (or a card built from the discard pile
  from the picker / discard-pile marker) to its tile or stage slot, fades in
  other players' new tiles, floats coin changes and slides the next hand in.
  Everything is skipped under `prefers-reduced-motion: reduce`.
  """
  use HeliosWeb, :html

  def fx_layer(assigns) do
    ~H"""
    <div
      id="fx-layer"
      phx-hook=".TurnFx"
      phx-update="ignore"
      class="pointer-events-none fixed inset-0 z-[70] overflow-hidden"
      aria-hidden="true"
    >
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".TurnFx">
      export default {
        mounted() {
          this.handleEvent("turn_resolved", (payload) => this.play(payload))
          window.addEventListener("fx:tokens", (e) => this.tokens(e.detail))
        },
        reduced() {
          return window.matchMedia("(prefers-reduced-motion: reduce)").matches
        },
        visible(el) {
          if (!el) return null
          const r = el.getBoundingClientRect()
          return r.width > 0 && r.height > 0 ? r : null
        },
        // A built tile inside a collapsed panel has no box; land on that
        // player's category chip instead (or their coins when the board has
        // no chip for that category).
        landTarget(player, target) {
          if (this.visible(target)) return target
          if (!target) return null
          const category = target.closest("[data-category]")?.dataset.category
          const board = `section[data-player="${player}"]`
          if (category) {
            const chip = document.querySelector(`${board} [data-chip="${category}"]`)
            if (chip) return chip
          }
          return document.querySelector(`${board} [data-stat="coins"]`)
        },
        play({plays, coins, me, my_card, pass}) {
          if (this.reduced()) return
          const rects = window.sevenWondersHandRects || {}
          const mine = my_card && rects[my_card]
          for (const play of plays) {
            const target = play.target && document.getElementById(play.target)
            const land = this.landTarget(play.player, target)
            if (play.from) {
              const from = this.origin(play.from, target)
              if (from && this.visible(land)) {
                this.fly(from, land, true)
                this.arrive(land, 550)
              } else if (land) {
                this.arrive(land)
              }
            } else if (play.player === me && mine) {
              const to = land || document.querySelector(`section[data-player="${me}"] [data-stat="coins"]`)
              this.fly(mine, to, !target)
            } else if (land) {
              this.arrive(land)
            }
          }
          for (const {player, delta} of coins) this.float(player, delta)
          if (pass) this.slideHand(pass)
        },
        // Where a card built from the discard pile takes off: the rect the
        // picker / replay grid snapshotted (.DiscardRects), else the status-bar
        // marker with the target tile's image. null → the caller only arrives.
        origin(id, target) {
          const saved = window.sevenWondersDiscardRects || {}
          if (saved[id]) {
            const rect = saved[id]
            delete saved[id]
            return rect
          }
          const pile = this.visible(document.getElementById("discard-pile"))
          const img = target && target.querySelector("img")
          if (!pile || !img) return null
          return {left: pile.left, top: pile.top, width: pile.width, height: pile.height, src: img.currentSrc || img.src}
        },
        fly(from, target, fade) {
          const to = this.visible(target)
          if (!to) return
          const ghost = document.createElement("img")
          ghost.src = from.src
          Object.assign(ghost.style, {
            position: "fixed", left: `${from.left}px`, top: `${from.top}px`,
            width: `${from.width}px`, height: `${from.height}px`, borderRadius: "8px",
            transformOrigin: "top left", boxShadow: "0 8px 24px rgba(0,0,0,.35)"
          })
          this.el.appendChild(ghost)
          const move = `translate(${to.left - from.left}px, ${to.top - from.top}px) scale(${to.width / from.width}, ${to.height / from.height})`
          ghost.animate([{transform: "none", opacity: 1}, {transform: move, opacity: fade ? 0 : 0.95}],
                        {duration: 700, easing: "cubic-bezier(.2,.8,.2,1)"})
            .finished.finally(() => ghost.remove())
          if (!fade) target.animate([{opacity: 0}, {opacity: 0, offset: 0.9}, {opacity: 1}], {duration: 750})
        },
        arrive(target, delay = 250) {
          if (!this.visible(target)) return
          target.animate([{transform: "scale(.6)", opacity: 0}, {transform: "scale(1)", opacity: 1}],
                         {duration: 500, delay, easing: "ease-out", fill: "backwards"})
        },
        float(player, delta) {
          const anchor = this.visible(document.querySelector(`section[data-player="${player}"] [data-stat="coins"]`))
          if (!anchor) return
          const label = document.createElement("span")
          label.textContent = delta > 0 ? `+${delta}` : `−${Math.abs(delta)}`
          Object.assign(label.style, {
            position: "fixed", left: `${anchor.left}px`, top: `${anchor.top - 6}px`, fontWeight: "800",
            color: delta > 0 ? "#15803d" : "#b91c1c", textShadow: "0 1px 2px rgba(255,255,255,.9)"
          })
          this.el.appendChild(label)
          label.animate([{transform: "translateY(0)", opacity: 1}, {transform: "translateY(-28px)", opacity: 0}],
                        {duration: 800, delay: 400, easing: "ease-out", fill: "backwards"})
            .finished.finally(() => label.remove())
        },
        slideHand(pass) {
          const hand = document.getElementById("hand") || document.getElementById("replay-hand")
          if (!hand) return
          const offset = pass === "west" ? 48 : -48
          hand.animate([{transform: `translateX(${offset}px)`, opacity: 0}, {transform: "none", opacity: 1}],
                       {duration: 400, delay: 700, easing: "ease-out", fill: "backwards"})
        },
        tokens(list) {
          if (this.reduced()) return
          for (const {player, src, rect} of list || []) {
            const board = `section[data-player="${player}"]`
            const target = document.querySelector(`${board} [data-tokens]`) ||
                           document.querySelector(`${board} [data-stat="coins"]`)
            this.fly({...rect, src}, target, true)
          }
        },
      }
    </script>
    """
  end
end

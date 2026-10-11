defmodule HeliosWeb.GameComponents.TopBar do
  @moduledoc """
  Age, turn, pass direction, who we are waiting for, and who is connected.

  Below `sm` the bar has three rows: age/history/turn/waiting button, then
  `#pass-row` (the pass direction), then the connection strip. From `sm` up
  everything sits in one wrapping row.
  """
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :view, :map, required: true
  attr :names, :map, required: true
  attr :connected, :any, required: true, doc: "MapSet of connected engine player ids"

  def top_bar(assigns) do
    assigns =
      assign(assigns,
        phase: assigns.view.phase,
        waiting: GameFormat.waiting_for(assigns.view, assigns.names),
        statuses: GameFormat.statuses(assigns.view),
        pass_to: GameFormat.player_name(assigns.names, GameFormat.pass_target(assigns.view))
      )

    ~H"""
    <header
      id="top-bar"
      data-phase={@phase.kind}
      data-turn-key={"#{@phase.age}-#{@phase.turn}"}
      class="sticky top-0 z-20 flex flex-col gap-1.5 rounded-xl bg-linear-to-r from-header-from to-header-to px-3 py-2 text-white shadow-lg short:static sm:flex-row sm:flex-wrap sm:items-center sm:justify-between sm:gap-3 sm:px-4 sm:py-3"
    >
      <div class="flex min-w-0 flex-col gap-1.5 sm:flex-row sm:items-center sm:gap-4">
        <div class="flex items-center justify-between gap-3 sm:justify-start">
          <div class="flex min-w-0 items-center gap-3 sm:gap-4">
            <span id="age-label" class="text-lg font-bold tracking-wide whitespace-nowrap sm:text-xl">
              Age {GameFormat.roman(@phase.age)}
            </span>
            <button
              id="history-button"
              type="button"
              phx-click={JS.toggle(to: "#history-drawer", display: "block")}
              aria-controls="history-drawer"
              class="inline-flex items-center justify-center gap-1 rounded-md bg-white/15 px-2 py-1 text-sm font-semibold transition hover:bg-white/25 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
            >
              <.icon name="hero-book-open" class="size-4" />
              <span class="sr-only sm:not-sr-only">
                History
              </span>
            </button>
            <%= if @phase.kind == :game_over do %>
              <span id="turn-label" class="whitespace-nowrap font-semibold">Game over</span>
            <% else %>
              <span id="turn-label" class="whitespace-nowrap">Turn {@phase.turn}/6</span>
            <% end %>
          </div>
          <.waiting_menu :if={@waiting != []} waiting={@waiting} />
        </div>
        <div
          :if={@phase.kind != :game_over}
          id="pass-row"
          class="flex min-w-0 items-center justify-between gap-3 text-sm sm:justify-start sm:text-base"
        >
          <span id="pass-direction" class="flex min-w-0 items-center gap-1">
            <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5 shrink-0" />
            <span class="min-w-0 break-words">Pass to {@pass_to}</span>
          </span>
        </div>
      </div>
      <p :if={@waiting != []} id="waiting-for" class="hidden text-sm text-white/90 sm:block">
        Waiting for: {Enum.join(@waiting, ", ")}
      </p>
      <ul
        id="connections"
        class="-mx-3 flex items-center gap-3 overflow-x-auto px-3 sm:mx-0 sm:flex-wrap sm:overflow-visible sm:px-0"
      >
        <li
          :for={player <- @view.players}
          id={"connection-#{player.name}"}
          data-connected={to_string(MapSet.member?(@connected, player.name))}
          data-status={@statuses[player.name]}
          class="flex shrink-0 items-center gap-1.5 text-sm"
        >
          <span class={[
            "size-2.5 shrink-0 rounded-full transition",
            if(MapSet.member?(@connected, player.name), do: "bg-emerald-400", else: "bg-disconnected")
          ]}>
          </span>
          <span class="max-w-32 truncate sm:max-w-none">
            {GameFormat.player_name(@names, player.name)}
          </span>
          <.icon
            :if={@statuses[player.name] == :chosen}
            name="hero-check"
            class="size-4 shrink-0 text-emerald-200"
          />
        </li>
      </ul>
    </header>
    """
  end

  attr :waiting, :list, required: true, doc: "display names of the players we wait for"

  # Phone-only "Waiting for N" button with a popover listing the names. Same
  # pattern as the stage popover (stage_strip.ex): JS.toggle, click-away and
  # Escape close it, no server state. Click-away sits on the wrapper so tapping
  # the button again toggles the list instead of closing and reopening it.
  defp waiting_menu(assigns) do
    ~H"""
    <div
      id="waiting-menu"
      class="relative sm:hidden"
      phx-click-away={JS.hide(to: "#waiting-list")}
      phx-window-keydown={JS.hide(to: "#waiting-list")}
      phx-key="Escape"
    >
      <button
        id="waiting-count"
        type="button"
        aria-haspopup="dialog"
        aria-controls="waiting-list"
        phx-click={JS.toggle(to: "#waiting-list", display: "block")}
        class="flex items-center gap-1 whitespace-nowrap rounded-md bg-white/15 px-2 py-1 text-sm text-white/90 transition hover:bg-white/25 pointer-coarse:min-h-11"
      >
        <.icon name="hero-clock" class="size-4" /> Waiting for {length(@waiting)}
      </button>
      <div
        id="waiting-list"
        role="dialog"
        aria-label="Waiting for"
        class="absolute top-full right-0 z-30 mt-2 hidden w-max max-w-[calc(100vw-2rem)] rounded-xl bg-white p-3 text-left text-sm text-zinc-800 shadow-2xl ring-1 ring-black/10"
      >
        <p class="text-xs font-semibold text-zinc-500">Waiting for</p>
        <ul class="mt-1 flex flex-col gap-1">
          <li :for={name <- @waiting} class="break-words">{name}</li>
        </ul>
      </div>
    </div>
    """
  end
end

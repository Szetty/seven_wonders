defmodule HeliosWeb.GameComponents.TopBar do
  @moduledoc "Age, turn, pass direction, who we are waiting for, and who is connected."
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :view, :map, required: true
  attr :names, :map, required: true
  attr :connected, :any, required: true, doc: "MapSet of connected engine player ids"

  def top_bar(assigns) do
    assigns =
      assign(assigns,
        phase: assigns.view.phase,
        waiting: GameFormat.waiting_for(assigns.view, assigns.names)
      )

    ~H"""
    <header
      id="top-bar"
      data-phase={@phase.kind}
      data-turn-key={"#{@phase.age}-#{@phase.turn}"}
      class="sticky top-0 z-20 flex flex-col gap-1.5 rounded-xl bg-linear-to-r from-header-from to-header-to px-3 py-2 text-white shadow-lg short:static sm:flex-row sm:flex-wrap sm:items-center sm:justify-between sm:gap-3 sm:px-4 sm:py-3"
    >
      <div class="flex items-center justify-between gap-3 sm:justify-start">
        <div class="flex items-center gap-3 sm:gap-4">
          <span id="age-label" class="text-lg font-bold tracking-wide sm:text-xl">
            Age {GameFormat.roman(@phase.age)}
          </span>
          <%= if @phase.kind == :game_over do %>
            <span id="turn-label" class="font-semibold">Game over</span>
          <% else %>
            <span id="turn-label">Turn {@phase.turn}/6</span>
            <span id="pass-direction" class="flex items-center gap-1">
              <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5" />
              <span class="sr-only sm:not-sr-only">
                Pass {GameFormat.direction_label(@phase.direction)}
              </span>
            </span>
          <% end %>
        </div>
        <span
          :if={@waiting != []}
          id="waiting-count"
          title={Enum.join(@waiting, ", ")}
          class="flex items-center gap-1 text-sm text-white/90 sm:hidden"
        >
          <.icon name="hero-clock" class="size-4" /> Waiting for {length(@waiting)}
        </span>
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
        </li>
      </ul>
    </header>
    """
  end
end

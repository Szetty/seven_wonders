defmodule HeliosWeb.GameComponents.TopBar do
  @moduledoc "Age, turn, pass direction, who we are waiting for, and who is connected."
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :view, :map, required: true
  attr :names, :map, required: true
  attr :connected, :any, required: true, doc: "MapSet of connected engine player ids"

  def top_bar(assigns) do
    assigns =
      assign(assigns, phase: assigns.view.phase, waiting: GameFormat.waiting_for(assigns.view, assigns.names))

    ~H"""
    <header
      id="top-bar"
      data-phase={@phase.kind}
      data-turn-key={"#{@phase.age}-#{@phase.turn}"}
      class="flex flex-wrap items-center justify-between gap-3 rounded-xl bg-linear-to-r from-header-from to-header-to px-4 py-3 text-white shadow-lg"
    >
      <div class="flex flex-wrap items-center gap-4">
        <span id="age-label" class="text-xl font-bold tracking-wide">Age {GameFormat.roman(@phase.age)}</span>
        <%= if @phase.kind == :game_over do %>
          <span id="turn-label" class="font-semibold">Game over</span>
        <% else %>
          <span id="turn-label">Turn {@phase.turn}/6</span>
          <span id="pass-direction" class="flex items-center gap-1">
            <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5" />
            Pass {GameFormat.direction_label(@phase.direction)}
          </span>
        <% end %>
      </div>
      <p :if={@waiting != []} id="waiting-for" class="text-sm text-white/90">
        Waiting for: {Enum.join(@waiting, ", ")}
      </p>
      <ul id="connections" class="flex flex-wrap items-center gap-3">
        <li
          :for={player <- @view.players}
          id={"connection-#{player.name}"}
          data-connected={to_string(MapSet.member?(@connected, player.name))}
          class="flex items-center gap-1.5 text-sm"
        >
          <span class={[
            "size-2.5 rounded-full transition",
            if(MapSet.member?(@connected, player.name), do: "bg-emerald-400", else: "bg-disconnected")
          ]}>
          </span>
          {GameFormat.player_name(@names, player.name)}
        </li>
      </ul>
    </header>
    """
  end
end

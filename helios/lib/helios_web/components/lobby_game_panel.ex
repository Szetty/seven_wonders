defmodule HeliosWeb.LobbyGamePanel do
  @moduledoc "Lobby controls for games: Start (owner), why it is disabled, the in-progress banner and Rejoin."
  use HeliosWeb, :html

  attr :owner?, :boolean, required: true
  attr :active_game, :any, default: nil
  attr :seated?, :boolean, default: false
  attr :start_blocker, :string, default: nil

  def game_panel(assigns) do
    ~H"""
    <section id="game-panel" class="flex flex-col gap-3">
      <div
        :if={@active_game}
        id="game-in-progress"
        class="flex items-center justify-between gap-3 rounded-xl bg-amber-50 px-4 py-3 ring-1 ring-amber-300"
      >
        <span class="flex items-center gap-2 font-semibold text-zinc-800">
          <.icon name="hero-play-circle" class="size-5 text-amber-600" /> Game in progress
        </span>
        <.link
          :if={@seated?}
          id="rejoin-game"
          navigate={~p"/game/#{@active_game.id}"}
          class="rounded-lg bg-zinc-900 px-3 py-1.5 text-sm font-semibold text-white transition hover:bg-zinc-700"
        >
          Rejoin
        </.link>
      </div>
      <div :if={@owner?} class="flex flex-wrap items-center gap-3">
        <button
          id="start-game"
          type="button"
          phx-click="start_game"
          disabled={@start_blocker != nil}
          class="rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white shadow transition hover:bg-zinc-700 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:bg-zinc-900"
        >
          Start game
        </button>
        <p :if={@start_blocker} id="start-blocker" class="text-sm text-zinc-600">{@start_blocker}</p>
      </div>
    </section>
    """
  end
end

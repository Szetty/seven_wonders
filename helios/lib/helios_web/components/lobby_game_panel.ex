defmodule HeliosWeb.LobbyGamePanel do
  @moduledoc "Lobby controls for games: the wonder-mode setting, Start (owner), why it is disabled, the in-progress banner and Rejoin."
  use HeliosWeb, :html

  @wonder_modes [
    {"random", "Random board & side"},
    {"choose_side", "Random board, choose side"},
    {"choose_board", "Choose board"}
  ]

  @doc "Label for a lobby `wonder_mode`."
  def wonder_mode_label(mode) do
    case List.keyfind(@wonder_modes, mode, 0) do
      {_mode, label} -> label
      nil -> mode
    end
  end

  attr :owner?, :boolean, required: true
  attr :active_game, :any, default: nil
  attr :seated?, :boolean, default: false
  attr :start_blocker, :string, default: nil
  attr :wonder_mode, :string, default: "random"

  def game_panel(assigns) do
    assigns = assign(assigns, :wonder_modes, @wonder_modes)

    ~H"""
    <section id="game-panel" class="flex flex-col gap-3">
      <div
        :if={@active_game}
        id="game-in-progress"
        class="flex flex-wrap items-center justify-between gap-3 rounded-xl bg-amber-50 px-4 py-3 ring-1 ring-amber-300"
      >
        <span class="flex items-center gap-2 font-semibold text-zinc-800">
          <.icon name="hero-play-circle" class="size-5 text-amber-600" /> Game in progress
        </span>
        <.link
          :if={@seated?}
          id="rejoin-game"
          navigate={~p"/game/#{@active_game.id}"}
          class="inline-flex items-center rounded-lg bg-zinc-900 px-3 py-1.5 text-sm font-semibold text-white transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11"
        >
          Rejoin
        </.link>
      </div>
      <div :if={@owner?} class="flex flex-col gap-1.5">
        <span id="wonder-mode-label" class="text-sm font-semibold text-zinc-700">
          Wonder boards
        </span>
        <div
          id="wonder-mode"
          role="group"
          aria-labelledby="wonder-mode-label"
          data-mode={@wonder_mode}
          class="flex flex-col overflow-hidden rounded-lg bg-white ring-1 ring-zinc-300 sm:flex-row sm:self-start"
        >
          <button
            :for={{mode, label} <- @wonder_modes}
            id={"wonder-mode-#{mode}"}
            type="button"
            phx-click="set_wonder_mode"
            phx-value-mode={mode}
            aria-pressed={to_string(@wonder_mode == mode)}
            class={[
              "px-3 py-2 text-sm font-semibold transition pointer-coarse:min-h-11",
              if(@wonder_mode == mode,
                do: "bg-zinc-900 text-white",
                else: "text-zinc-800 hover:bg-zinc-100"
              )
            ]}
          >
            {label}
          </button>
        </div>
      </div>
      <p :if={!@owner?} id="wonder-mode" data-mode={@wonder_mode} class="text-sm text-zinc-700">
        Wonder boards: <span class="font-semibold">{wonder_mode_label(@wonder_mode)}</span>
      </p>
      <div :if={@owner?} class="flex flex-wrap items-center gap-3">
        <button
          id="start-game"
          type="button"
          phx-click="start_game"
          disabled={@start_blocker != nil}
          class="rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white shadow transition hover:bg-zinc-700 active:bg-zinc-800 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:bg-zinc-900 pointer-coarse:min-h-11"
        >
          Start game
        </button>
        <p :if={@start_blocker} id="start-blocker" class="text-sm text-zinc-600">{@start_blocker}</p>
      </div>
    </section>
    """
  end
end

defmodule HeliosWeb.GameComponents.ReplayBar do
  @moduledoc "Turn-by-turn navigation through a finished game (frames from `Helios.Games.replay/2`)."
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :frame, :map, required: true
  attr :index, :integer, required: true
  attr :last, :integer, required: true
  attr :playing, :boolean, required: true

  def replay_bar(assigns) do
    ~H"""
    <div
      id="replay-bar"
      phx-hook=".ReplayAutoplay"
      data-playing={to_string(@playing)}
      class="flex flex-col gap-2 rounded-xl bg-antique/95 p-2 shadow sm:flex-row sm:items-center sm:justify-between"
    >
      <div class="flex flex-wrap items-center justify-center gap-1">
        <.control
          id="replay-first"
          event="replay_first"
          icon="hero-backward"
          label="First turn"
          disabled={@index == 0}
        />
        <.control
          id="replay-prev"
          event="replay_prev"
          icon="hero-chevron-left"
          label="Previous turn"
          disabled={@index == 0}
        />
        <span id="replay-position" class="min-w-36 text-center font-semibold text-zinc-900">
          {position(@frame.phase)}
        </span>
        <.control
          id="replay-next"
          event="replay_next"
          icon="hero-chevron-right"
          label="Next turn"
          disabled={@index == @last}
        />
        <.control
          id="replay-last"
          event="replay_last"
          icon="hero-forward"
          label="Final table"
          disabled={@index == @last}
        />
        <.control
          id="replay-play"
          event="replay_toggle"
          icon={if @playing, do: "hero-pause", else: "hero-play"}
          label={if @playing, do: "Pause", else: "Play"}
          disabled={@index == @last}
        />
      </div>
      <p :if={@frame.my_choice} id="replay-choice" class="text-center text-sm text-zinc-700">
        You chose: <strong>{GameFormat.describe_action(@frame.my_choice)}</strong>
      </p>
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".ReplayAutoplay">
      export default {
        mounted() { this.sync() },
        updated() { this.sync() },
        destroyed() { clearInterval(this.timer) },
        sync() {
          const playing = this.el.dataset.playing === "true"
          if (playing && !this.timer) this.timer = setInterval(() => this.pushEvent("replay_next", {}), 2500)
          if (!playing && this.timer) { clearInterval(this.timer); this.timer = null }
        },
      }
    </script>
    """
  end

  attr :id, :string, required: true
  attr :event, :string, required: true
  attr :icon, :string, required: true
  attr :label, :string, required: true
  attr :disabled, :boolean, default: false

  defp control(assigns) do
    ~H"""
    <button
      id={@id}
      type="button"
      phx-click={@event}
      disabled={@disabled}
      aria-label={@label}
      class="inline-flex size-9 items-center justify-center rounded-full text-zinc-800 transition hover:bg-white disabled:cursor-not-allowed disabled:opacity-40 pointer-coarse:size-11"
    >
      <.icon name={@icon} class="size-5" />
    </button>
    """
  end

  def position(%{kind: :game_over}), do: "Final"

  def position(%{kind: :extra_turn, age: age, turn: turn}),
    do: "Age #{GameFormat.roman(age)} · Turn #{turn} · extra turn"

  def position(%{age: age, turn: turn}), do: "Age #{GameFormat.roman(age)} · Turn #{turn}"
end

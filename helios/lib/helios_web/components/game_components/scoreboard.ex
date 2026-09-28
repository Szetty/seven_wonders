defmodule HeliosWeb.GameComponents.Scoreboard do
  @moduledoc "Final scores, winner highlighted, with a link back to the lobby."
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  @columns [
    military: "Military",
    treasury: "Treasury",
    wonder: "Wonder",
    civilian: "Civilian",
    scientific: "Science",
    commercial: "Commercial",
    guild: "Guilds",
    total: "Total"
  ]

  attr :scores, :list, required: true
  attr :names, :map, required: true
  attr :me, :string, required: true
  attr :lobby_id, :string, required: true

  def scoreboard(assigns) do
    assigns =
      assign(assigns, columns: @columns, rows: Enum.sort_by(assigns.scores, &{&1.rank, -&1.total}))

    ~H"""
    <section id="scoreboard" class="mx-auto w-full max-w-4xl rounded-xl bg-antique/95 p-6 shadow-xl">
      <h2 class="mb-4 text-center text-2xl font-bold text-zinc-900">Final scores</h2>
      <div class="overflow-x-auto">
        <table class="w-full text-center text-sm">
          <thead class="border-b border-zinc-300 text-xs uppercase tracking-wide text-zinc-600">
            <tr>
              <th class="px-2 py-2 text-left">Player</th>
              <th :for={{_key, label} <- @columns} class="px-2 py-2">{label}</th>
              <th class="px-2 py-2">Rank</th>
            </tr>
          </thead>
          <tbody>
            <tr
              :for={score <- @rows}
              id={"score-row-#{score.player}"}
              data-rank={score.rank}
              class={[
                "border-b border-zinc-200",
                score.rank == 1 && "bg-amber-200 font-semibold",
                score.player == @me && "outline-2 outline-sky-500"
              ]}
            >
              <td class="px-2 py-2 text-left">
                <.icon :if={score.rank == 1} name="hero-trophy" class="size-4 text-amber-600" />
                {GameFormat.player_name(@names, score.player)}
              </td>
              <td :for={{key, _label} <- @columns} class="px-2 py-2">{Map.fetch!(score, key)}</td>
              <td class="px-2 py-2">{score.rank}</td>
            </tr>
          </tbody>
        </table>
      </div>
      <div class="mt-6 flex justify-center">
        <.link
          id="back-to-lobby"
          navigate={~p"/lobby/#{@lobby_id}"}
          class="rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white transition hover:bg-zinc-700"
        >
          Back to lobby
        </.link>
      </div>
    </section>
    """
  end
end

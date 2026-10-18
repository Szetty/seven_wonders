defmodule HeliosWeb.PastGamesLive do
  @moduledoc "Finished games the current user played, newest first, linking to their final table."
  use HeliosWeb, :live_view

  alias Helios.Games

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app
      flash={@flash}
      current_scope={@current_scope}
      notifications={@notifications}
      recent_tables={@recent_tables}
      background="bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed"
    >
      <section id="past-games" class="mx-auto w-full max-w-4xl px-2 py-4 sm:px-4">
        <h1 class="mb-3 text-xl font-bold text-zinc-900">Past games</h1>
        <p :if={@rows == []} id="past-games-empty" class="rounded-xl bg-antique/90 p-4 text-zinc-700">
          No finished games yet.
        </p>
        <ul :if={@rows != []} class="flex flex-col gap-2">
          <li :for={row <- @rows} id={"past-game-#{row.id}"}>
            <.link
              navigate={~p"/game/#{row.id}"}
              class="block rounded-xl bg-antique/90 p-3 shadow transition hover:-translate-y-0.5 hover:bg-antique active:bg-amber-100"
            >
              <div class="flex flex-wrap items-baseline justify-between gap-2">
                <span class="font-semibold text-zinc-900">{row.date}</span>
                <span :if={row.my_rank} data-my-rank class="text-sm text-zinc-700">
                  Rank {row.my_rank} · {row.my_total} points
                </span>
              </div>
              <p class="mt-1 flex flex-wrap gap-x-3 gap-y-1 text-sm text-zinc-700">
                <span :for={player <- row.players} data-winner={player.winner?}>
                  <.icon :if={player.winner?} name="hero-trophy" class="size-4 text-amber-600" />{player.name}
                </span>
              </p>
            </.link>
          </li>
        </ul>
      </section>
    </Layouts.app>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    user = socket.assigns.current_scope.user
    rows = user |> Games.finished_games_for() |> Enum.map(&row(&1, to_string(user.id)))
    {:ok, assign(socket, page_title: "Past games", rows: rows)}
  end

  defp row(game, me) do
    scores = Map.new(get_in(game.final_scores || %{}, ["scores"]) || [], &{&1["player"], &1})
    mine = Map.get(scores, me, %{})
    names = Games.display_names(game.players)

    %{
      id: game.id,
      date: Calendar.strftime(game.finished_at, "%d %b %Y, %H:%M"),
      my_rank: mine["rank"],
      my_total: mine["total"],
      players:
        Enum.map(game.players, fn player ->
          id = Games.player_id(player)
          %{name: Map.fetch!(names, id), winner?: get_in(scores, [id, "rank"]) == 1}
        end)
    }
  end
end

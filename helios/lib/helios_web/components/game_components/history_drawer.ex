defmodule HeliosWeb.GameComponents.HistoryDrawer do
  @moduledoc "What happened so far, newest first, grouped by age. Toggled client-side from the top bar."
  use HeliosWeb, :html

  alias HeliosWeb.{GameFormat, HistoryText}

  attr :history, :list, required: true
  attr :ctx, :map, required: true
  attr :frames, :map, default: %{}, doc: "event index => replay frame index (finished games)"

  def history_drawer(assigns) do
    assigns =
      assign(assigns,
        ages:
          assigns.history
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.chunk_by(fn {{_tag, event}, _i} -> event.age end)
      )

    ~H"""
    <aside
      id="history-drawer"
      role="dialog"
      aria-labelledby="history-title"
      phx-window-keydown={JS.hide(to: "#history-drawer")}
      phx-key="Escape"
      class="fixed inset-x-0 bottom-0 z-50 hidden max-h-[70dvh] overflow-y-auto rounded-t-2xl bg-white p-4 pb-[max(1rem,env(safe-area-inset-bottom))] text-sm text-zinc-800 shadow-2xl ring-1 ring-black/5 lg:inset-x-auto lg:top-4 lg:right-4 lg:bottom-4 lg:max-h-none lg:w-96 lg:rounded-2xl"
    >
      <header class="mb-2 flex items-center justify-between">
        <h2 id="history-title" class="text-base font-semibold">History</h2>
        <button
          id="close-history"
          type="button"
          aria-label="Close"
          phx-click={JS.hide(to: "#history-drawer")}
          class="inline-flex size-9 items-center justify-center rounded-full text-zinc-500 hover:bg-zinc-100 pointer-coarse:size-11"
        >
          <.icon name="hero-x-mark" class="size-5" />
        </button>
      </header>
      <p :if={@history == []} id="history-empty" class="text-zinc-500">Nothing has happened yet.</p>
      <section :for={[{{_tag, first}, _i} | _] = events <- @ages} data-age={first.age} class="mb-3">
        <h3 class="mb-1 text-xs font-extrabold uppercase tracking-wide text-zinc-500">
          Age {GameFormat.roman(first.age)}
        </h3>
        <%= for {{tag, event}, index} <- events do %>
          <div
            :if={tag == :battles}
            id={"history-#{index}"}
            data-history-index={index}
            class="mb-2 rounded-lg bg-red-50 px-2 py-1.5 ring-1 ring-red-200"
          >
            <p class="font-semibold">Military conflicts</p>
            <p :for={battle <- event.battles}>{HistoryText.battle(battle, @ctx)}</p>
          </div>
          <div
            :if={tag == :turn}
            id={"history-#{index}"}
            data-history-index={index}
            class="mb-2 border-l-4 border-zinc-200 pl-2"
          >
            <%= if Map.has_key?(@frames, index) do %>
              <button
                type="button"
                phx-click="replay_goto"
                phx-value-frame={@frames[index]}
                class="text-xs font-semibold text-sky-700 underline-offset-2 hover:underline"
              >
                {HistoryText.turn_title(event)}
              </button>
            <% else %>
              <p class="text-xs font-semibold text-zinc-500">{HistoryText.turn_title(event)}</p>
            <% end %>
            <p :for={play <- event.plays} class="flex flex-wrap items-center gap-1">
              {HistoryText.play(play, @ctx)}
              <span
                :if={random_play?(@ctx, event, play)}
                data-random
                class="rounded bg-amber-100 px-1 text-xs font-semibold text-amber-800"
              >
                random
              </span>
              <button
                :if={card_of(play)}
                type="button"
                phx-click="show_card"
                phx-value-card={card_of(play)}
                phx-value-owner={play.player}
                aria-label={"About #{card_of(play)}"}
                class="inline-flex items-center justify-center text-zinc-400 hover:text-zinc-700 pointer-coarse:size-11"
              >
                <.icon name="hero-information-circle" class="size-4" />
              </button>
            </p>
            <p :if={event.coins != []} class="text-xs text-zinc-600">
              {HistoryText.coins(event.coins, @ctx)}
            </p>
          </div>
        <% end %>
      </section>
    </aside>
    """
  end

  defp card_of(%{action: {kind, card}}) when kind in [:built, :built_free, :built_from_discard],
    do: card

  defp card_of(_play), do: nil

  defp random_play?(ctx, event, play),
    do: Helios.Games.RandomMoves.random?(Map.get(ctx, :random, MapSet.new()), event, play.player)
end

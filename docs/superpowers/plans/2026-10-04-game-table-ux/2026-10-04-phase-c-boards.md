# Game Table UX — Phase C: Everyone's Boards Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every player's full board is on the table (West · You · East, then the others), the latest built card glows gold, any built card can be read, the Halikarnassós discard picker can be minimised to look at the boards, finished games show their boards, and a "Past games" page lists finished games.

**Architecture:** One `PlayerBoard` component replaces `Board.my_board`, `neighbour_panel` and `others_strip`. A read-only `CardSheet` reuses Phase B's `CardInfo` and Phase A's `CardZoom`. Finished games are rebuilt by a new `Helios.Games.Replay` module (extracted from `GameServer.init/1`) without starting a GameServer. `PastGamesLive` lists finished games.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1, Ecto (SQLite, portable queries), Tailwind 4.1, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md` (section "Phase C — Everyone's boards").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`.
- Elixir never re-implements rules; boards show only what the engine view contains.
- Hidden information stays on the server: a finished game's page uses the viewer's own view (`Core.view(ref, user_id)`), never another player's.
- Kept ids: `#my-board`, `#my-built`, `#west-panel`, `#east-panel` (+ `-toggle`/`-details`), `#other-players` with one `article#player-<id>` per other player, `#wonder-stages`, `#scoreboard`, `#discard-picker`, `#discard-pick-N`. Changed: `#built-<slug>` → `#built-<player>-<slug>` on every board.
- Ecto queries stay portable (no SQLite-only SQL). Only one active game per lobby exists (unique partial index): tests must finish a game before creating another in the same lobby.
- Touch targets ≥ 44×44 CSS px; no horizontal scroll at ≥ 360px; `responsive.spec.ts` must stay green.
- Stage explicit paths only; never stage `helios/config/config.exs` / `helios/config/dev.exs`.
- `cd helios && mix precommit` before each commit.

## Review Focus

- A finished game whose action log does not reach game over (e.g. rows deleted, or the existing test fixture with no actions) must fall back to the scoreboard with `#boards-unavailable`, not render a turn-1 table under "Game over". (Test in Task 4.)
- A finished game whose log no longer replays (corrupt action row) must not crash the page and must not abort or modify the game row. (Test in Task 4.)
- With 4 players, exactly one other board; with 3 players, no `#other-players` element; with 7, four others in seat order continuing from the east neighbour. (Test in Task 2.)
- A player who built nothing has no `data-latest` tile and the board still renders (empty `#my-built`). (Test in Task 1.)
- Past games shows only games the viewer was seated in, never aborted or active ones, newest first. (Test in Task 5.)

---

### Task 1: `PlayerBoard` and `CardSheet` components

**Files:**
- Create: `helios/lib/helios_web/components/game_components/player_board.ex`, `helios/lib/helios_web/components/game_components/card_sheet.ex`
- Modify: `helios/lib/helios_web/game_format.ex` (`others/1`, `neighbours_of/3`)
- Test: `helios/test/helios_web/components/game_components/player_board_test.exs` (new), `card_sheet_test.exs` (new), `helios/test/helios_web/game_format_test.exs`

**Interfaces:**
- Consumes: `PlayerPanels.player_stats/1`, `PlayerPanels.status_chip/1`, `Icons.military_icon/1` (Phase A); `StageStrip.stage_strip/1`, `CardInfo.card_info/1` (Phase B); `CardZoom.card_zoom/1`, `CardZoom.show_zoom/0` (Phase A).
- Produces: `PlayerBoard.player_board/1` — attrs `id` (string), `player` (public player map), `names` (map), `label` (string: "You", "West", "East" or ""), `status` (atom | nil), `mine` (boolean, default false), `class` (extra classes). Tiles are `button#built-<player>-<slug>` with `data-card`, `phx-click="show_card"`, `phx-value-card`, `phx-value-owner`; the last built card has `data-latest`. Section carries `data-player`.
- Produces: `CardSheet.card_sheet/1` — attrs `name`, `neighbours` (map), `built` (viewer's built names). Ids `#card-sheet`, `#card-sheet-scrim`, `#close-card-sheet`; events `hide_card`.
- Produces: `GameFormat.others(view) :: [player]` (seat order continuing from the east neighbour, excluding me/west/east) and `GameFormat.neighbours_of(view, player_id, names) :: %{west: String.t(), east: String.t()}`.

- [ ] **Step 1: Write the failing tests**

`player_board_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.PlayerBoardTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.PlayerBoard

  defp board(player, opts) do
    render_component(&PlayerBoard.player_board/1,
      Keyword.merge(
        [player: player, names: SampleViews.names(), status: :thinking, label: "East", id: "east-panel"],
        opts
      )
    )
  end

  test "a neighbour's full board: wonder, stages, stats, tiles, latest" do
    html = board(SampleViews.player("2", %{wonder: "Rhódos", side: :b}), [])
    assert attrs(html, "#east-panel", "data-player") == ["2"]
    assert attrs(html, "#east-panel-details img[alt='Rhódos B']", "src") == ["/images/wonders/rhodosB.png"]
    assert count(html, "#wonder-stages-2 [data-built]") == 3
    assert text(html, "#east-panel-details [data-stat='coins']") == "4"
    assert text(html, "#east-panel-details [data-stat='military']") == "1"
    assert attrs(html, "#built-cards-2 [data-card]", "data-card") == ["Lumber Yard", "Altar", "Stockade"]
    assert attrs(html, "#built-2-stockade", "phx-click") == ["show_card"]
    assert attrs(html, "#built-2-stockade", "phx-value-owner") == ["2"]
    assert attrs(html, "#east-panel [data-latest]", "data-card") == ["Stockade"]
    assert attrs(html, "#status-2", "data-status") == ["thinking"]
    assert text(html, "#east-panel-details header") =~ "Bob"
  end

  test "non-own boards fold into a summary toggle below md" do
    html = board(SampleViews.player("2"), [])
    assert attrs(html, "#east-panel-toggle", "aria-controls") == ["east-panel-details"]
    assert [js] = attrs(html, "#east-panel-toggle", "phx-click")
    assert js =~ "is-open"
    assert count(html, "#east-panel-toggle [data-stat], #east-panel-toggle [data-card]") == 0
  end

  test "my board keeps its ids, has no toggle, and shows no name" do
    html = board(SampleViews.player("1"), id: "my-board", label: "You", mine: true)
    assert count(html, "#my-board-toggle") == 0
    assert count(html, "#wonder-stages") == 1
    assert attrs(html, "#my-built [data-card]", "data-card") == ["Lumber Yard", "Altar", "Stockade"]
    assert count(html, "#built-1-altar") == 1
  end

  test "an empty city renders without a latest tile" do
    html = board(SampleViews.player("1", %{built: []}), id: "my-board", label: "You", mine: true)
    assert count(html, "#my-built [data-card]") == 0
    assert count(html, "[data-latest]") == 0
  end
end
```

`card_sheet_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.CardSheetTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardSheet

  test "a read-only sheet: art with zoom, explanation, no actions, closable" do
    html = render_component(&CardSheet.card_sheet/1, name: "Temple", neighbours: %{}, built: [])
    assert attrs(html, "#card-sheet", "role") == ["dialog"]
    assert text(html, "#card-sheet-title") == "Temple"
    assert count(html, "#card-sheet #card-info") == 1
    assert count(html, "#card-sheet #zoom-card") == 1
    assert count(html, "#card-sheet [phx-click='submit']") == 0
    assert attrs(html, "#card-sheet-scrim", "phx-click") == ["hide_card"]
    assert attrs(html, "#close-card-sheet", "phx-click") == ["hide_card"]
    assert attrs(html, "#card-sheet", "phx-window-keydown") == ["hide_card"]
  end
end
```

`game_format_test.exs`:

```elixir
  test "other players continue round the table from the east neighbour" do
    players = for id <- ~w(1 2 3 4 5 6 7), do: %{name: id}
    view = %{me: "3", players: players}
    assert Enum.map(GameFormat.others(view), & &1.name) == ["5", "6", "7", "1"]
    assert GameFormat.others(%{me: "1", players: Enum.take(players, 3)}) == []
    assert Enum.map(GameFormat.others(%{me: "1", players: Enum.take(players, 4)}), & &1.name) == ["3"]

    assert GameFormat.neighbours_of(view, "1", %{"7" => "Gus", "2" => "Bob"}) ==
             %{west: "Gus", east: "Bob"}
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/player_board_test.exs test/helios_web/components/game_components/card_sheet_test.exs test/helios_web/game_format_test.exs`
Expected: FAIL (modules/functions undefined).

- [ ] **Step 3: `GameFormat` helpers**

```elixir
  @doc "Players other than me and my neighbours, in seat order starting after my east neighbour."
  def others(%{players: players, me: me}) do
    count = length(players)
    index = Enum.find_index(players, &(&1.name == me))
    for k <- 2..(count - 2)//1, do: Enum.at(players, rem(index + k, count))
  end

  @doc "Display names of `player`'s west and east neighbours."
  def neighbours_of(%{players: players}, player, names) do
    count = length(players)
    index = Enum.find_index(players, &(&1.name == player))

    %{
      west: player_name(names, Enum.at(players, rem(index - 1 + count, count)).name),
      east: player_name(names, Enum.at(players, rem(index + 1, count)).name)
    }
  end
```

- [ ] **Step 4: `PlayerBoard`**

`helios/lib/helios_web/components/game_components/player_board.ex`:

```elixir
defmodule HeliosWeb.GameComponents.PlayerBoard do
  @moduledoc """
  One player's city: wonder with its stage strip, stats, and every built card
  as a tile (the latest one glows gold). Renders your board, your neighbours'
  and everyone else's; boards other than yours fold into a one-line summary
  below `md` (toggle via `JS.toggle_class("is-open")`, no server state).
  """
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.Icons, only: [military_icon: 1]
  import HeliosWeb.GameComponents.PlayerPanels, only: [player_stats: 1, status_chip: 1]
  import HeliosWeb.GameComponents.StageStrip, only: [stage_strip: 1]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :id, :string, required: true
  attr :player, :map, required: true
  attr :names, :map, required: true
  attr :label, :string, required: true
  attr :status, :atom, default: nil
  attr :mine, :boolean, default: false
  attr :class, :any, default: nil

  def player_board(assigns) do
    player = assigns.player
    name = if assigns.mine, do: "You", else: GameFormat.player_name(assigns.names, player.name)

    assigns =
      assign(assigns,
        name: name,
        columns: GameFormat.group_by_category(player.built),
        latest: List.last(player.built),
        strip_id: if(assigns.mine, do: "wonder-stages", else: "wonder-stages-#{player.name}"),
        built_id: if(assigns.mine, do: "my-built", else: "built-cards-#{player.name}")
      )

    ~H"""
    <section
      id={@id}
      data-player={@player.name}
      class={["group min-w-0 flex flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow sm:p-3", @class]}
    >
      <button
        :if={!@mine}
        id={"#{@id}-toggle"}
        type="button"
        aria-expanded="false"
        aria-controls={"#{@id}-details"}
        phx-click={
          JS.toggle_class("is-open", to: "##{@id}")
          |> JS.toggle_attribute({"aria-expanded", "true", "false"})
        }
        class="flex w-full items-center gap-2 text-left transition active:opacity-70 md:hidden pointer-coarse:min-h-11"
      >
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt=""
          class="aspect-[16/5] w-18 shrink-0 rounded object-cover"
        />
        <span class="flex min-w-0 flex-1 flex-col">
          <span class="flex items-baseline gap-1.5">
            <span :if={@label != ""} class="text-xs font-semibold uppercase tracking-wide text-zinc-500">
              {@label}
            </span>
            <span class="truncate font-semibold text-zinc-900">{@name}</span>
            <span
              :if={@status}
              data-status-dot={@status}
              class={["size-2 shrink-0 rounded-full", if(@status == :chosen, do: "bg-emerald-500", else: "bg-sky-400")]}
            >
            </span>
          </span>
          <span class="flex items-center gap-2 text-xs text-zinc-700">
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:coin)} alt="" class="size-3.5" />{@player.coins}
            </span>
            <span class="flex items-center gap-0.5" title="Military strength">
              <.military_icon class="size-3.5" />{@player.shields}
            </span>
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:pyramid)} alt="" class="size-3.5" />{@player.stages_built}/{@player.stages_total}
            </span>
            <span class="flex min-w-0 flex-wrap gap-0.5">
              <span
                :for={card <- @player.built}
                class={["size-2.5 rounded-sm", GameFormat.category_class(card.category)]}
              >
              </span>
            </span>
          </span>
        </span>
        <.icon
          name="hero-chevron-down"
          class="size-4 shrink-0 text-zinc-500 transition group-[.is-open]:rotate-180"
        />
      </button>
      <div
        id={"#{@id}-details"}
        class={["flex-col gap-2", if(@mine, do: "flex", else: "hidden group-[.is-open]:flex md:flex")]}
      >
        <header class="flex items-center justify-between gap-2">
          <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
          <.status_chip id={"status-#{@player.name}"} status={@status} />
          <span :if={!@mine} class="min-w-0 truncate font-semibold text-zinc-900">{@name}</span>
        </header>
        <div class="relative overflow-hidden rounded-lg">
          <img
            src={GameAssets.wonder_path(@player.wonder, @player.side)}
            alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
            class="aspect-[16/5] w-full object-cover"
          />
          <.stage_strip id={@strip_id} player={@player} owner={@name} />
        </div>
        <.player_stats player={@player} />
        <div id={@built_id} class="flex gap-2 overflow-x-auto pb-1">
          <div
            :for={{category, cards} <- @columns}
            data-category={category}
            class="flex shrink-0 flex-col gap-1"
          >
            <button
              :for={card <- cards}
              id={"built-#{@player.name}-#{GameAssets.slug(card.name)}"}
              type="button"
              data-card={card.name}
              data-latest={card == @latest}
              title={card.name}
              phx-click="show_card"
              phx-value-card={card.name}
              phx-value-owner={@player.name}
              class={[
                "h-10 w-16 overflow-hidden rounded ring-2 transition hover:-translate-y-0.5 pointer-coarse:min-h-11",
                if(card == @latest,
                  do: "ring-amber-400 shadow-[0_0_10px_rgba(245,158,11,0.9)]",
                  else: GameFormat.category_ring(category)
                )
              ]}
            >
              <img src={GameAssets.card_path(card.name)} alt={card.name} class="w-full object-cover object-top" />
            </button>
          </div>
        </div>
      </div>
    </section>
    """
  end
end
```

- [ ] **Step 5: `CardSheet`**

`helios/lib/helios_web/components/game_components/card_sheet.ex`:

```elixir
defmodule HeliosWeb.GameComponents.CardSheet do
  @moduledoc "Read-only card details (any built card on any board)."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.CardInfo, only: [card_info: 1]
  import HeliosWeb.GameComponents.CardZoom, only: [card_zoom: 1, show_zoom: 0]

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :name, :string, required: true
  attr :neighbours, :map, required: true
  attr :built, :list, required: true

  def card_sheet(assigns) do
    ~H"""
    <div id="card-sheet-scrim" phx-click="hide_card" class="fixed inset-0 z-40 bg-black/35" aria-hidden="true"></div>
    <section
      id="card-sheet"
      role="dialog"
      aria-modal="true"
      aria-labelledby="card-sheet-title"
      phx-window-keydown="hide_card"
      phx-key="Escape"
      class={GameFormat.sheet_class()}
    >
      <button
        id="close-card-sheet"
        type="button"
        phx-click="hide_card"
        aria-label="Close"
        class="absolute right-2 top-2 inline-flex size-9 items-center justify-center rounded-full text-zinc-500 transition hover:bg-zinc-100 hover:text-zinc-900 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-5" />
      </button>
      <div class="flex items-start gap-3 sm:gap-4">
        <button
          id="zoom-card"
          type="button"
          phx-click={show_zoom()}
          aria-label={"Enlarge #{@name}"}
          class="shrink-0 cursor-zoom-in rounded-lg"
        >
          <img src={GameAssets.card_path(@name)} alt={@name} class="h-[147px] w-24 rounded-lg sm:h-[183px] sm:w-[120px]" />
        </button>
        <div class="flex min-w-0 flex-1 flex-col gap-2">
          <h3 id="card-sheet-title" class="pr-10 text-lg font-semibold text-zinc-900">{@name}</h3>
          <.card_info name={@name} neighbours={@neighbours} built={@built} />
        </div>
      </div>
      <.card_zoom name={@name} />
    </section>
    """
  end
end
```

- [ ] **Step 6: Run and commit**

Run: `cd helios && mix test test/helios_web/components/game_components test/helios_web/game_format_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/player_board.ex helios/lib/helios_web/components/game_components/card_sheet.ex helios/lib/helios_web/game_format.ex helios/test/helios_web/components/game_components/player_board_test.exs helios/test/helios_web/components/game_components/card_sheet_test.exs helios/test/helios_web/game_format_test.exs
git commit -m "feat(web): PlayerBoard for every city and a read-only card sheet"
```

---

### Task 2: The table shows every board

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex`
- Delete: `helios/lib/helios_web/components/game_components/board.ex`, `helios/test/helios_web/components/game_components/board_test.exs`
- Modify: `helios/lib/helios_web/components/game_components/player_panels.ex` (remove `neighbour_panel/1` and `others_strip/1`; keep `player_stats/1`, `status_chip/1`), `helios/test/helios_web/components/game_components/player_panels_test.exs` (keep only `status_chip` and `player_stats` tests)
- Test: `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: Task 1 components and helpers.
- Produces: GameLive events `show_card` (`%{"card" => name, "owner" => player_id}`) and `hide_card`; assign `info_card :: %{name: String.t(), owner: String.t()} | nil`. Phase D's history drawer sends `show_card` too.

- [ ] **Step 1: Write the failing tests**

In `game_live_test.exs`, add a setup helper for larger tables and tests:

```elixir
  defp table_of(count) do
    players = for _ <- 1..count, do: user_with_token_fixture()
    {players, game_fixture(Enum.map(players, &elem(&1, 0)))}
  end

  test "every board is on the table; others continue from the east neighbour" do
    {players, game} = table_of(5)
    [{_a, ta} | _] = players
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {:ok, view, _html} = open(ta, game.id)

    assert has_element?(view, "#my-board[data-player='#{Enum.at(ids, 0)}']")
    assert has_element?(view, "#east-panel[data-player='#{Enum.at(ids, 1)}']")
    assert has_element?(view, "#west-panel[data-player='#{Enum.at(ids, 4)}']")

    html = render(view)
    others = html |> LazyHTML.from_fragment() |> LazyHTML.query("#other-players article") |> LazyHTML.attribute("id")
    assert others == ["player-#{Enum.at(ids, 2)}", "player-#{Enum.at(ids, 3)}"]
    assert has_element?(view, "#player-#{Enum.at(ids, 2)} [id^='wonder-stages-']")
  end

  test "a built card anywhere opens a read-only card sheet", %{game: game, players: players} do
    [{a, ta}, {b, _}, {c, _}] = players
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    ids = Enum.map([a, b, c], &to_string(&1.id))
    Helios.GameDriver.step(ids, Map.new(ids, &{&1, :build}), view_fun, submit_fun)

    {:ok, view, _html} = open(ta, game.id)
    tile = view |> element("#east-panel [data-latest]")

    if has_element?(view, "#east-panel [data-latest]") do
      render_click(tile)
      assert has_element?(view, "#card-sheet #card-info")
      view |> element("#close-card-sheet") |> render_click()
      refute has_element?(view, "#card-sheet")
    end
  end
```

(`GameDriver.step/4` with `:build` builds a card when one is buildable; if the deal gives the east neighbour nothing buildable the tile assertion is skipped — the component test in Task 1 covers the tile itself.)

Keep "a seated player sees the table" (3 players: `refute has_element?(view, "#other-players")`).

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/game_live_test.exs`
Expected: FAIL (`#other-players` is still the old strip above the table with summaries; no `show_card`).

- [ ] **Step 3: Rewrite the table in `GameLive`**

Imports: drop `HeliosWeb.GameComponents.Board` and the `PlayerPanels` import; add

```elixir
  import HeliosWeb.GameComponents.CardSheet
  import HeliosWeb.GameComponents.PlayerBoard
```

Replace the `#table` div with a private component call `<.table_area …/>` and define it in GameLive:

```elixir
  attr :me, :map, required: true
  attr :west, :map, required: true
  attr :east, :map, required: true
  attr :others, :list, required: true
  attr :names, :map, required: true
  attr :statuses, :map, required: true

  defp table_area(assigns) do
    ~H"""
    <div id="table" class="flex flex-1 flex-col gap-3 pb-1 sm:gap-4">
      <div class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-[1fr_1.6fr_1fr]">
        <.player_board id="west-panel" label="West" player={@west} names={@names} status={@statuses[@west.name]} />
        <.player_board
          id="my-board"
          label="You"
          player={@me}
          names={@names}
          status={@statuses[@me.name]}
          mine
          class="order-first md:col-span-2 lg:order-none lg:col-span-1"
        />
        <.player_board id="east-panel" label="East" player={@east} names={@names} status={@statuses[@east.name]} />
      </div>
      <section
        :if={@others != []}
        id="other-players"
        class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-4"
      >
        <article :for={player <- @others} id={"player-#{player.name}"} class="min-w-0">
          <.player_board
            id={"board-#{player.name}"}
            label=""
            player={player}
            names={@names}
            status={@statuses[player.name]}
          />
        </article>
      </section>
    </div>
    """
  end
```

In `render/1` the non-game-over branch now starts with:

```heex
            <.table_area me={@me} west={@west} east={@east} others={@others} names={@names} statuses={@statuses} />
```

and after `<.action_panel …>` add:

```heex
            <.card_sheet
              :if={@info_card}
              name={@info_card.name}
              neighbours={GameFormat.neighbours_of(@view, @info_card.owner, @names)}
              built={Enum.map(@me.built, & &1.name)}
            />
```

In `assign_view/2` compute `others: GameFormat.others(view)` (replacing the `Enum.reject` line). In `mount_active`/`mount_finished` (via `setup/3`) assign `info_card: nil`.

Events:

```elixir
  def handle_event("show_card", %{"card" => card, "owner" => owner}, socket)
      when is_binary(card) and is_binary(owner) do
    known? = Enum.any?(socket.assigns.view.players, &(&1.name == owner and Enum.any?(&1.built, fn b -> b.name == card end)))
    info = if known?, do: %{name: card, owner: owner}
    {:noreply, socket |> assign_selection(nil) |> assign(:info_card, info)}
  end

  def handle_event("hide_card", %{"key" => key}, socket) when key != "Escape", do: {:noreply, socket}
  def handle_event("hide_card", _params, socket), do: {:noreply, assign(socket, :info_card, nil)}
```

(Only cards actually on the table can be opened — a forged event names nothing.)

- [ ] **Step 4: Delete the old components and their tests**

Delete `board.ex` and `board_test.exs`. From `player_panels.ex` remove `neighbour_panel/1` and `others_strip/1` (and the `JS` alias if unused); update its `@moduledoc` to "Shared player widgets: the stats row and the Chosen/Thinking chip." From `player_panels_test.exs` remove the three panel/strip tests (their assertions moved to `player_board_test.exs`); keep the status chip test.

- [ ] **Step 5: Run everything, including e2e**

```bash
cd helios && mix test && mix precommit
cd ../e2e && npx playwright test
```

Expected: PASS. `responsive.spec.ts` uses `#west-panel-toggle`/`#west-panel-details` (kept), `#other-players article` (4 with 7 players) and `#my-built` (kept). `game.spec.ts` `builtCards` reads `#my-built [data-card]` (kept).

- [ ] **Step 6: Commit**

```bash
git add helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/test/helios_web/components/game_components/player_panels_test.exs helios/test/helios_web/live/game_live_test.exs
git rm helios/lib/helios_web/components/game_components/board.ex helios/test/helios_web/components/game_components/board_test.exs
git commit -m "feat(web): show every player's full board on the table"
```

---

### Task 3: Minimise the discard picker to look at the boards

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/extra_turn.ex`, `helios/lib/helios_web/live/game_live.ex`
- Test: `helios/test/helios_web/components/game_components/extra_turn_test.exs`, `helios/test/helios_web/live/game_live_extra_turns_test.exs`

**Interfaces:**
- Produces: `ExtraTurn.discard_picker/1` attr `minimised` (boolean); `ExtraTurn.discard_restore/1` (dock bar); `ExtraTurn.my_discard_turn?/1` made public. GameLive events `minimise_discard`, `restore_discard`; assign `discard_minimised`.

- [ ] **Step 1: Write the failing tests**

`extra_turn_test.exs`:

```elixir
  test "the picker can be minimised to a bar that restores it" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar"], hand: []})
    open = render_component(&ExtraTurn.discard_picker/1, view: view, minimised: false)
    assert attrs(open, "#discard-minimise", "phx-click") == ["minimise_discard"]

    closed = render_component(&ExtraTurn.discard_picker/1, view: view, minimised: true)
    assert count(closed, "#discard-picker") == 0
    assert count(closed, "#discard-scrim") == 0

    bar = render_component(&ExtraTurn.discard_restore/1, view: view, minimised: true)
    assert attrs(bar, "#discard-restore", "phx-click") == ["restore_discard"]
    assert render_component(&ExtraTurn.discard_restore/1, view: view, minimised: false) =~ ~r/^\s*$/
  end
```

Update the other `render_picker/1` calls in that file to pass `minimised: false` (`defp render_picker(view), do: render_component(&ExtraTurn.discard_picker/1, view: view, minimised: false)`).

`game_live_extra_turns_test.exs` — in "Halikarnassós builds from the discard pile…", before picking:

```elixir
    view_a |> element("#discard-minimise") |> render_click()
    refute has_element?(view_a, "#discard-picker")
    assert has_element?(view_a, "#dock #discard-restore")
    send(view_a.pid, {:game_updated, 0})
    assert has_element?(view_a, "#dock #discard-restore")
    view_a |> element("#discard-restore") |> render_click()
    assert has_element?(view_a, "#discard-picker")
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/extra_turn_test.exs test/helios_web/live/game_live_extra_turns_test.exs`
Expected: FAIL.

- [ ] **Step 3: Implement**

`extra_turn.ex`: `discard_picker/1` gains `attr :minimised, :boolean, default: false`; render only when `my_discard_turn?(@view) and not @minimised`. In the sheet header:

```heex
        <div class="mb-3 flex items-center justify-between gap-2">
          <h2 id="discard-picker-title" class="font-semibold text-zinc-900">
            Build one card from the discard pile for free
          </h2>
          <button
            id="discard-minimise"
            type="button"
            phx-click="minimise_discard"
            class="inline-flex shrink-0 items-center gap-1 rounded-lg px-2 py-1 text-sm font-semibold text-sky-700 transition hover:bg-sky-50 pointer-coarse:min-h-11"
          >
            <.icon name="hero-eye" class="size-4" /> Look at the boards
          </button>
        </div>
```

Add:

```elixir
  attr :view, :map, required: true
  attr :minimised, :boolean, required: true

  def discard_restore(assigns) do
    ~H"""
    <div
      :if={my_discard_turn?(@view) and @minimised}
      class="flex items-center justify-between gap-2 rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300"
    >
      <span>Pick a card from the discard pile</span>
      <button
        id="discard-restore"
        type="button"
        phx-click="restore_discard"
        class="inline-flex items-center rounded-lg bg-zinc-900 px-3 py-1.5 font-semibold text-white transition hover:bg-zinc-700 pointer-coarse:min-h-11"
      >
        Show
      </button>
    </div>
    """
  end
```

and make `my_discard_turn?/1` public (`def`).

`game_live.ex`: in the dock, after `<.extra_turn_notice …/>`: `<.discard_restore view={@view} minimised={@discard_minimised} />`; pass `minimised={@discard_minimised}` to `<.discard_picker>`. Assign `discard_minimised: false` in `setup/3`; in `assign_view/2` reset it when it is not the viewer's discard turn:

```elixir
    |> assign(:discard_minimised, socket.assigns[:discard_minimised] == true and ExtraTurn.my_discard_turn?(view))
```

(alias `HeliosWeb.GameComponents.ExtraTurn`). Events:

```elixir
  def handle_event("minimise_discard", _params, socket), do: {:noreply, assign(socket, :discard_minimised, true)}
  def handle_event("restore_discard", _params, socket), do: {:noreply, assign(socket, :discard_minimised, false)}
```

- [ ] **Step 4: Run and commit**

Run: `cd helios && mix test && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/extra_turn.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/extra_turn_test.exs helios/test/helios_web/live/game_live_extra_turns_test.exs
git commit -m "feat(web): minimise the discard picker to look at the boards"
```

---

### Task 4: `Games.Replay` and boards at game over

**Files:**
- Create: `helios/lib/helios/games/replay.ex`
- Modify: `helios/lib/helios/games/game_server.ex` (`init/1` uses `Replay`), `helios/lib/helios/games.ex` (`replay/2`), `helios/lib/helios_web/live/game_live.ex` (game-over branch, `mount_finished`)
- Test: `helios/test/helios/games/replay_test.exs` (new), `helios/test/helios_web/live/game_live_test.exs`, `helios/test/helios_web/live/game_live_extra_turns_test.exs`

**Interfaces:**
- Produces: `Helios.Games.Replay.player_ids(game_id) :: [String.t()]`, `Replay.actions(game_id) :: [GameAction.t()]`, `Replay.rebuild(game, actions, init, step) :: {:ok, Core.game(), acc} | {:error, term()}` where `init :: (Core.game() -> acc)` runs once on the fresh engine and `step :: (GameAction.t(), Core.game(), acc -> acc)` runs after each replayed row. Phase D builds replay frames with it.
- Produces: `Games.replay(game, user_id) :: {:ok, %{view: view}} | {:error, term()}` (`{:error, :not_finished}` when the log does not reach game over). Phase D extends the map with `history` and `frames`.
- Produces: GameLive assign `boards_available :: boolean`; `#boards-unavailable`.

- [ ] **Step 1: Write the failing tests**

`helios/test/helios/games/replay_test.exs`:

```elixir
defmodule Helios.Games.ReplayTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games, Repo}
  alias Helios.Games.{Game, GameAction, Replay}

  setup do
    on_exit(&stop_game_servers/0)
    users = for _ <- 1..3, do: player_fixture()
    game = game_fixture(users)
    ids = Enum.map(users, &to_string(&1.id))
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)
    {:game_over, live} = GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
    %{game: Repo.get!(Game, game.id), users: users, ids: ids, live: live}
  end

  test "rebuild replays every row and reports each step", %{game: game} do
    rows = Replay.actions(game.id)
    assert {:ok, _ref, count} = Replay.rebuild(game, rows, fn _ref -> 0 end, fn _row, _ref, n -> n + 1 end)
    assert count == length(rows)
  end

  test "replay/2 rebuilds the viewer's final view without a GameServer", %{game: game, users: [a | _], live: live} do
    stop_game_servers()
    assert game.status == "finished"
    assert {:ok, %{view: view}} = Games.replay(game, a.id)
    assert view.phase.kind == :game_over
    assert view.me == to_string(a.id)
    assert Enum.map(view.players, & &1.built) == Enum.map(live.players, & &1.built)
    assert Registry.lookup(Helios.Games.Registry, game.id) == []
  end

  test "a log that no longer replays is an error and leaves the game untouched", %{game: game, users: [a | _]} do
    Repo.insert!(%GameAction{game_id: game.id, seq: 10_000, user_id: a.id, action: %{"type" => "discard", "card" => "Not A Card"}})
    assert {:error, {:replay_failed, 10_000, _}} = Games.replay(game, a.id)
    assert Repo.get!(Game, game.id).status == "finished"
  end
end
```

`game_live_test.exs` — the existing "a finished game shows its persisted scoreboard without starting the engine" (no action rows) now also asserts the fallback:

```elixir
    assert has_element?(view, "#boards-unavailable")
    refute has_element?(view, "#table")
```

and add:

```elixir
  test "a finished game shows the scoreboard and every final board", %{game: game, players: players} do
    [{a, ta} | _] = players
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    {:game_over, _} = Helios.GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
    stop_game_servers()

    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "#scoreboard")
    assert has_element?(view, "#table #my-board[data-player='#{a.id}']")
    assert has_element?(view, "#table #west-panel")
    refute has_element?(view, "#dock")
    refute has_element?(view, "[data-status]")
    refute has_element?(view, "#boards-unavailable")
  end
```

`game_live_extra_turns_test.exs` — "the scoreboard replaces the table at game end…" becomes "the scoreboard heads the final table at game end…": replace nothing about `#hand`/`#dock`/`#action-panel` (still refuted) and add `assert has_element?(view_a, "#table #my-board")`.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games/replay_test.exs test/helios_web/live`
Expected: FAIL (`Replay` undefined, `Games.replay/2` undefined, no `#table` at game over).

- [ ] **Step 3: Extract `Replay`**

`helios/lib/helios/games/replay.ex`:

```elixir
defmodule Helios.Games.Replay do
  @moduledoc """
  Rebuilds a game's engine from `seed` + `wonders` + the action log. Used by
  `GameServer.init/1` and, for finished games, by `Helios.Games.replay/2`
  (a throwaway engine, never registered as a GameServer).
  """
  import Ecto.Query

  alias Helios.{Core, Repo}
  alias Helios.Games.{ActionCodec, Game, GameAction, GamePlayer}

  @spec player_ids(Ecto.UUID.t()) :: [String.t()]
  def player_ids(game_id) do
    from(p in GamePlayer, where: p.game_id == ^game_id, order_by: p.seat, select: p.user_id)
    |> Repo.all()
    |> Enum.map(&to_string/1)
  end

  @spec actions(Ecto.UUID.t()) :: [GameAction.t()]
  def actions(game_id),
    do: Repo.all(from a in GameAction, where: a.game_id == ^game_id, order_by: a.seq)

  @doc """
  A fresh engine with every row of `actions` submitted in order. `init` sees
  the fresh engine and returns the accumulator; `step` sees each row with the
  engine just after it was applied.
  """
  @spec rebuild(Game.t(), [GameAction.t()], (Core.game() -> acc), (GameAction.t(), Core.game(), acc -> acc)) ::
          {:ok, Core.game(), acc} | {:error, term()}
        when acc: term()
  def rebuild(%Game{} = game, actions, init, step) do
    with {:ok, wonders} <- ActionCodec.decode_wonders(game.wonders),
         {:ok, ref} <- Core.new_game(player_ids(game.id), wonders, game.seed) do
      actions
      |> Enum.reduce_while({:ok, init.(ref)}, fn %GameAction{} = row, {:ok, acc} ->
        with {:ok, action} <- ActionCodec.decode(row.action),
             :ok <- Core.submit(ref, to_string(row.user_id), action) do
          {:cont, {:ok, step.(row, ref, acc)}}
        else
          {:error, reason} -> {:halt, {:error, {:replay_failed, row.seq, reason}}}
        end
      end)
      |> case do
        {:ok, acc} -> {:ok, ref, acc}
        error -> error
      end
    end
  end
end
```

`game_server.ex` `init/1`: replace the player/action queries and `replay/2` with `Replay`:

```elixir
  def init(opts) do
    game = Repo.get!(Game, Keyword.fetch!(opts, :game_id))
    idle_timeout = Keyword.fetch!(opts, :idle_timeout)
    actions = Replay.actions(game.id)
    warn_on_engine_version(game)

    case Replay.rebuild(game, actions, fn _ref -> nil end, fn _row, _ref, acc -> acc end) do
      {:ok, ref, nil} ->
        state = %{
          game_id: game.id,
          lobby_id: game.lobby_id,
          ref: ref,
          seq: last_seq(actions),
          players: Replay.player_ids(game.id),
          status: game.status,
          idle_timeout: idle_timeout
        }

        state = maybe_finish(state)
        {:ok, state, state.idle_timeout}

      {:error, reason} ->
        Logger.error("Game #{game.id} could not be rebuilt (#{inspect(reason)}); marking it aborted")
        abort(game)
        :ignore
    end
  end
```

Delete the private `replay/2` (now unused) and add `Replay` to the alias list. `GameServerTest` must pass unchanged.

`games.ex`:

```elixir
  alias Helios.Games.Replay

  @doc """
  Rebuilds a finished game from its log in a throwaway engine and returns the
  viewer's final view. `{:error, :not_finished}` if the log stops short of game over.
  """
  @spec replay(Game.t(), integer()) :: {:ok, %{view: map()}} | {:error, term()}
  def replay(%Game{} = game, user_id) do
    with {:ok, ref, nil} <- Replay.rebuild(game, Replay.actions(game.id), fn _ -> nil end, fn _, _, acc -> acc end),
         {:ok, %{phase: %{kind: :game_over}} = view} <- Core.view(ref, to_string(user_id)) do
      {:ok, %{view: view}}
    else
      {:ok, _view} -> {:error, :not_finished}
      {:error, reason} -> {:error, reason}
    end
  end
```

- [ ] **Step 4: GameLive at game over**

`mount_finished/3`:

```elixir
  defp mount_finished(socket, game, user) do
    socket = setup(socket, game, user)

    case Games.replay(game, user.id) do
      {:ok, %{view: view}} ->
        {:ok, socket |> assign(:boards_available, true) |> assign_view(view)}

      {:error, reason} ->
        Logger.warning("Game #{game.id} boards unavailable: #{inspect(reason)}")
        {:ok, socket |> assign(:boards_available, false) |> assign_finished_view(game, user)}
    end
  end
```

(`require Logger` at the top.) In `setup/3` assign `boards_available: true`. The game-over branch of `render/1`:

```heex
          <%= if @view.phase.kind == :game_over do %>
            <div class="flex flex-col gap-3 pb-4 sm:gap-4">
              <.scoreboard scores={@view.scores} names={@names} me={@view.me} lobby_id={@game.lobby_id} />
              <p
                :if={!@boards_available}
                id="boards-unavailable"
                class="mx-auto rounded-lg bg-antique/90 px-3 py-2 text-sm text-zinc-700"
              >
                Boards are unavailable for this game
              </p>
              <.table_area
                :if={@boards_available}
                me={@me}
                west={@west}
                east={@east}
                others={@others}
                names={@names}
                statuses={@statuses}
              />
              <.card_sheet
                :if={@info_card}
                name={@info_card.name}
                neighbours={GameFormat.neighbours_of(@view, @info_card.owner, @names)}
                built={Enum.map(@me.built, & &1.name)}
              />
            </div>
          <% else %>
```

(`GameFormat.statuses/1` returns nil for everyone at game over, so no chips render.)

- [ ] **Step 5: Run and commit**

Run: `cd helios && mix test && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios/games/replay.ex helios/lib/helios/games/game_server.ex helios/lib/helios/games.ex helios/lib/helios_web/live/game_live.ex helios/test/helios/games/replay_test.exs helios/test/helios_web/live/game_live_test.exs helios/test/helios_web/live/game_live_extra_turns_test.exs
git commit -m "feat(games): rebuild finished games to show their final boards"
```

---

### Task 5: Past games page

**Files:**
- Create: `helios/lib/helios_web/live/past_games_live.ex`
- Modify: `helios/lib/helios/games.ex` (`finished_games_for/2`), `helios/lib/helios_web/router.ex`, `helios/lib/helios_web/components/layouts.ex` (`site_header/1`)
- Modify: `e2e/tests/game.spec.ts`
- Test: `helios/test/helios/games_test.exs`, `helios/test/helios_web/live/past_games_live_test.exs` (new), `helios/test/helios_web/components/layouts_test.exs`

**Interfaces:**
- Produces: `Games.finished_games_for(user, limit \\ 50) :: [Game.t()]` (players + users preloaded); route `live "/games", PastGamesLive`; header link `#past-games-link`; rows `#past-game-<id>`, empty state `#past-games-empty`.

- [ ] **Step 1: Write the failing tests**

`games_test.exs`:

```elixir
  describe "finished_games_for/2" do
    test "lists only my finished games, newest first" do
      [me, b, c] = users = for _ <- 1..3, do: player_fixture()
      lobby = Helios.Lobbies.get_or_create_own_lobby(me)

      finish = fn finished_at ->
        game = game_fixture(users, lobby: lobby)
        game |> Ecto.Changeset.change(status: "finished", finished_at: finished_at, final_scores: %{"scores" => []}) |> Repo.update!()
      end

      old = finish.(~U[2026-01-01 10:00:00Z])
      new = finish.(~U[2026-02-01 10:00:00Z])
      aborted = game_fixture(users, lobby: lobby) |> Ecto.Changeset.change(status: "aborted") |> Repo.update!()
      _active = game_fixture(users, lobby: lobby)

      [x, y, z] = for _ <- 1..3, do: player_fixture()
      other_lobby = Helios.Lobbies.get_or_create_own_lobby(x)
      game_fixture([x, y, z], lobby: other_lobby)
      |> Ecto.Changeset.change(status: "finished", finished_at: ~U[2026-03-01 10:00:00Z])
      |> Repo.update!()

      assert Enum.map(Games.finished_games_for(me), & &1.id) == [new.id, old.id]
      refute aborted.id in Enum.map(Games.finished_games_for(me), & &1.id)
      assert [%{players: [%{user: %{name: _}} | _]} | _] = Games.finished_games_for(me)
      assert length(Games.finished_games_for(me, 1)) == 1
      assert Games.finished_games_for(b) |> length() == 2
      assert Games.finished_games_for(c) |> length() == 2
    end
  end
```

(Add `import Helios.GamesFixtures` / `alias Helios.Repo` if `games_test.exs` lacks them.)

`past_games_live_test.exs`:

```elixir
defmodule HeliosWeb.PastGamesLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.Repo

  test "lists my finished games with rank and links to them" do
    [{a, ta}, {b, _}, {c, _}] = players = for _ <- 1..3, do: user_with_token_fixture()
    game = game_fixture(Enum.map(players, &elem(&1, 0)))

    scores = %{
      "scores" => [
        %{"player" => to_string(b.id), "total" => 40, "rank" => 1},
        %{"player" => to_string(a.id), "total" => 31, "rank" => 2},
        %{"player" => to_string(c.id), "total" => 20, "rank" => 3}
      ]
    }

    game
    |> Ecto.Changeset.change(status: "finished", finished_at: ~U[2026-02-01 10:00:00Z], final_scores: scores)
    |> Repo.update!()

    {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/games")
    assert has_element?(view, "#past-game-#{game.id} a[href='/game/#{game.id}']")
    assert has_element?(view, "#past-game-#{game.id} [data-my-rank]", "Rank 2")
    assert has_element?(view, "#past-game-#{game.id} [data-winner]", b.name)
    refute has_element?(view, "#past-games-empty")
  end

  test "shows an empty state" do
    {_user, token} = user_with_token_fixture()
    {:ok, view, _html} = live(log_in_conn(build_conn(), token), ~p"/games")
    assert has_element?(view, "#past-games-empty")
  end
end
```

`layouts_test.exs`: in the header test add

```elixir
    assert doc |> LazyHTML.query("#site-header #past-games-link") |> LazyHTML.attribute("href") == ["/games"]
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games_test.exs test/helios_web/live/past_games_live_test.exs test/helios_web/components/layouts_test.exs`
Expected: FAIL.

- [ ] **Step 3: Query**

`games.ex`:

```elixir
  @spec finished_games_for(Helios.Accounts.User.t(), pos_integer()) :: [Game.t()]
  def finished_games_for(%{id: user_id}, limit \\ 50) do
    Repo.all(
      from g in Game,
        join: p in GamePlayer,
        on: p.game_id == g.id and p.user_id == ^user_id,
        where: g.status == "finished",
        order_by: [desc: g.finished_at, desc: g.inserted_at],
        limit: ^limit,
        preload: [players: :user]
    )
  end
```

- [ ] **Step 4: LiveView, route, header link**

`router.ex` — inside `live_session :authenticated`: `live "/games", PastGamesLive`.

`layouts.ex` `site_header/1` — before `#my-table-link`:

```heex
      <.link
        navigate={~p"/games"}
        id="past-games-link"
        class="inline-flex shrink-0 items-center justify-center rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        Past games
      </.link>
```

`helios/lib/helios_web/live/past_games_live.ex`:

```elixir
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

    %{
      id: game.id,
      date: Calendar.strftime(game.finished_at, "%d %b %Y, %H:%M"),
      my_rank: mine["rank"],
      my_total: mine["total"],
      players:
        Enum.map(game.players, fn player ->
          id = to_string(player.user_id)
          %{name: player.user.name, winner?: get_in(scores, [id, "rank"]) == 1}
        end)
    }
  end
end
```

(`data-winner={false}` omits the attribute, so `[data-winner]` matches only winners.)

- [ ] **Step 5: e2e — open the finished game from Past games**

In `e2e/tests/game.spec.ts` "three players play a full game to the scoreboard", before closing the contexts:

```ts
  const owner = players[0];
  await owner.page.locator("#past-games-link").click();
  await expect(owner.page).toHaveURL(/\/games$/);
  const row = owner.page.locator("[id^='past-game-']").first();
  await expect(row).toBeVisible();
  await row.locator("a").click();
  await expect(owner.page.locator("#scoreboard")).toBeVisible();
  await expect(owner.page.locator("#table #my-board")).toBeVisible();
```

(`owner` is already destructured from `setupTable` in that test — reuse it instead of redeclaring.)

- [ ] **Step 6: Phase gate and commit**

```bash
cd helios && mix precommit
cd ../e2e && npx playwright test
```

Expected: all green. On the 360px phone header (`responsive.spec.ts` "lobby header and notifications fit"), `#my-table-link` and `#logout-link` must stay fully in the viewport; if the extra link pushes them out, shorten the label below `sm` (`<span class="sm:hidden">Games</span><span class="hidden sm:inline">Past games</span>`).

```bash
git add helios/lib/helios_web/live/past_games_live.ex helios/lib/helios/games.ex helios/lib/helios_web/router.ex helios/lib/helios_web/components/layouts.ex helios/test/helios/games_test.exs helios/test/helios_web/live/past_games_live_test.exs helios/test/helios_web/components/layouts_test.exs e2e/tests/game.spec.ts
git commit -m "feat(web): Past games page listing finished games"
```

# Game Table UX — Phase A: Quick Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the Helios-only polish items of the game table: pointer cursor, centred hand, background behind the header, game-style military icon, wonder stages on top, Chosen/Thinking status, neighbour names in the pass direction and trade options, card cost instead of "Bank", and an enlarged card view.

**Architecture:** Pure presentation changes in `HeliosWeb.GameComponents.*`, `HeliosWeb.GameFormat`, `HeliosWeb.Layouts` and `assets/css/app.css`. No engine, NIF or context change. Everything is server-rendered; the card zoom uses `Phoenix.LiveView.JS` only.

**Tech Stack:** Phoenix 1.8, LiveView 1.1, Tailwind CSS 4.1.12, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md` (section "Phase A — Quick fixes").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`; it takes precedence there (HEEx rules, `<.icon>`, no inline `<script>`, `Layouts.app` first in every LiveView).
- All game rules stay in `core`; Elixir only formats what the view already contains.
- DOM ids used by tests stay unless the spec's "Id changes" table lists them: in this phase only `[data-stat="shields"]` → `[data-stat="military"]`.
- Touch targets stay ≥ 44×44 CSS px on touch viewports (`pointer-coarse:min-h-11` / `min-w-11` on new buttons).
- No page may scroll horizontally at ≥ 360px wide.
- Stage explicit paths only (never `git add -A`/`.`); never stage `helios/config/config.exs` or `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit that touches `helios/`.

## Review Focus

- Long player names (e.g. 30 characters) in the pass direction and status chips must truncate, not widen the top bar or panels (phone top bar stays ≤ 96px tall — `responsive.spec.ts` asserts it).
- A trade option that pays only one neighbour must not show "Bob 0"; a card with no coin cost must not show a card-cost line.
- An extra turn (Babylon B / Halikarnassós) shows "Playing…" only on the acting player and no Chosen/Thinking chips elsewhere; game over shows no chips.
- Pressing Escape with the zoom open must not crash or leave the zoom visible.
- The card zoom must not appear on page load (hidden until clicked) and must not be in the tab order while hidden.

---

### Task 1: Pointer cursor, centred hand, background behind the header

**Files:**
- Modify: `helios/assets/css/app.css`
- Modify: `helios/lib/helios_web/components/layouts.ex` (`app/1`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1`)
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`hand/1`)
- Modify: `helios/lib/helios_web/components/game_components/extra_turn.ex` (`discard_picker/1`)
- Test: `helios/test/helios_web/design_base_test.exs`, `helios/test/helios_web/components/layouts_test.exs`, `helios/test/helios_web/components/game_components/hand_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Produces: `Layouts.app/1` attr `background` (`:string`, default `nil`) — a class string applied to the layout's outermost div. Phase C's `PastGamesLive` uses it.

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios_web/design_base_test.exs` (inside the module):

```elixir
  test "app.css restores the pointer cursor on interactive elements" do
    css = File.read!("assets/css/app.css")
    assert css =~ ~s(button:not(:disabled))
    assert css =~ ~s([role="button"]:not([aria-disabled="true"]))
    assert css =~ "cursor: pointer"
  end
```

In `helios/test/helios_web/components/layouts_test.exs` replace `render_app/1` with a version that accepts a background and add a test:

```elixir
  defp render_app(scope, background \\ nil) do
    assigns = %{scope: scope, background: background}

    rendered_to_string(~H"""
    <Layouts.app flash={%{}} current_scope={@scope} background={@background}>
      <p id="content">Hello</p>
    </Layouts.app>
    """)
    |> LazyHTML.from_fragment()
  end

  test "the background class wraps the header and the content" do
    doc = render_app(Scope.for_user(%User{id: 1, name: "alice"}), "bg-test-paper")
    assert doc |> LazyHTML.query(".bg-test-paper #site-header") |> Enum.count() == 1
    assert doc |> LazyHTML.query(".bg-test-paper main #content") |> Enum.count() == 1
  end
```

Append to `helios/test/helios_web/components/game_components/hand_test.exs`:

```elixir
  test "the hand row centres its cards without clipping the first one" do
    html = render_component(&Hand.hand/1, hand: SampleViews.hand(), selected: nil, pending: nil)
    assert [class] = attrs(html, "#hand > div", "class")
    assert class =~ "justify-center-safe"
  end
```

Append to `helios/test/helios_web/live/game_live_test.exs`:

```elixir
  test "the paper background covers the whole page, header included", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "[class*='paper.jpg'] #site-header")
    refute has_element?(view, "#game[class*='paper.jpg']")
  end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/design_base_test.exs test/helios_web/components/layouts_test.exs test/helios_web/components/game_components/hand_test.exs test/helios_web/live/game_live_test.exs`
Expected: the four new tests FAIL (no cursor rule; `background` attr unknown / class missing; no `justify-center-safe`; paper background still on `#game`).

- [ ] **Step 3: Implement**

`helios/assets/css/app.css` — after the `@custom-variant short …` line add:

```css
/* Tailwind v4 preflight gives buttons the default cursor; restore the pointer
   on everything clickable (disabled buttons keep cursor-not-allowed). */
@layer base {
  button:not(:disabled),
  [role="button"]:not([aria-disabled="true"]),
  a[href],
  label[for],
  summary {
    cursor: pointer;
  }
}
```

`helios/lib/helios_web/components/layouts.ex` — in `app/1` add the attr after `notifications` and use it on the outer div:

```elixir
  attr :background, :string,
    default: nil,
    doc: "classes painting the page background behind the header and the content"
```

```heex
    <div class={["min-h-dvh", @background]}>
```

`helios/lib/helios_web/live/game_live.ex` — move the background from `#game` to the layout:

```heex
    <Layouts.app
      flash={@flash}
      current_scope={@current_scope}
      notifications={@notifications}
      background="bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed"
    >
      <div id="game" class="flex min-h-dvh flex-col">
```

`helios/lib/helios_web/components/game_components/hand.ex` — the row inside `#hand`:

```heex
      <div class="-mx-2 flex snap-x justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1 sm:pt-3">
```

`helios/lib/helios_web/components/game_components/extra_turn.ex` — the picker grid:

```heex
        <div class="grid grid-cols-3 gap-2 sm:flex sm:flex-wrap sm:justify-center">
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/design_base_test.exs test/helios_web/components/layouts_test.exs test/helios_web/components/game_components/hand_test.exs test/helios_web/live/game_live_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
git add helios/assets/css/app.css helios/lib/helios_web/components/layouts.ex helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/components/game_components/extra_turn.ex helios/test/helios_web/design_base_test.exs helios/test/helios_web/components/layouts_test.exs helios/test/helios_web/components/game_components/hand_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "feat(web): pointer cursor, centred hand, paper background behind the header"
```

---

### Task 2: Military strength icon and wonder stages on top

**Files:**
- Create: `helios/lib/helios_web/components/game_components/icons.ex`
- Modify: `helios/lib/helios_web/components/game_components/player_panels.ex` (summary toggle + `player_stats/1`)
- Modify: `helios/lib/helios_web/components/game_components/board.ex` (`#wonder-stages`)
- Test: `helios/test/helios_web/components/game_components/icons_test.exs` (new), `player_panels_test.exs`, `board_test.exs`

**Interfaces:**
- Produces: `HeliosWeb.GameComponents.Icons.military_icon/1` — attrs `class` (default `"size-5"`), global `rest`; renders an `<svg data-icon="military" aria-hidden="true">`. Used again in Phases C and D.
- Produces: `[data-stat="military"]` replaces `[data-stat="shields"]`.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios_web/components/game_components/icons_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.IconsTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.Icons

  test "the military icon is a decorative inline svg" do
    html = render_component(&Icons.military_icon/1, class: "size-4")
    assert attrs(html, "svg[data-icon='military']", "aria-hidden") == ["true"]
    assert attrs(html, "svg[data-icon='military']", "class") == ["size-4"]
  end
end
```

In `player_panels_test.exs` change the shields assertion and add icon checks to the first test:

```elixir
    assert text(html, "#east-panel [data-stat='military']") == "1"
    assert count(html, "#east-panel-details svg[data-icon='military']") == 1
    assert attrs(html, "#east-panel-details [title='Military strength']", "title") == ["Military strength"]
    assert count(html, "#east-panel .hero-shield-check") == 0
```

In `board_test.exs` add:

```elixir
  test "built stages sit on the top edge of the wonder image" do
    html = render_component(&Board.my_board/1, player: SampleViews.player("1"))
    assert [class] = attrs(html, "#wonder-stages", "class")
    assert class =~ "top-0"
    refute class =~ "bottom-0"
  end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/icons_test.exs test/helios_web/components/game_components/player_panels_test.exs test/helios_web/components/game_components/board_test.exs`
Expected: FAIL (`Icons` undefined; `data-stat='military'` missing; `bottom-0` present).

- [ ] **Step 3: Implement**

Create `helios/lib/helios_web/components/game_components/icons.ex`:

```elixir
defmodule HeliosWeb.GameComponents.Icons do
  @moduledoc "Game symbols that heroicons has no match for."
  use HeliosWeb, :html

  attr :class, :any, default: "size-5"
  attr :rest, :global

  @doc "The red shield of military strength, as printed on military cards."
  def military_icon(assigns) do
    ~H"""
    <svg
      data-icon="military"
      viewBox="0 0 20 24"
      class={@class}
      aria-hidden="true"
      {@rest}
    >
      <path
        d="M10 1.5 L18.5 4.5 V11 C18.5 16.5 15 20.5 10 22.5 C5 20.5 1.5 16.5 1.5 11 V4.5 Z"
        fill="#b91c1c"
        stroke="#d4a017"
        stroke-width="2"
      />
      <path d="M10 6 V18 M6 11 H14" stroke="#fde68a" stroke-width="1.6" stroke-linecap="round" />
    </svg>
    """
  end
end
```

`player_panels.ex` — add `import HeliosWeb.GameComponents.Icons, only: [military_icon: 1]` after the aliases. In the summary toggle replace

```heex
            <span class="flex items-center gap-0.5">
              <.icon name="hero-shield-check" class="size-3.5 text-red-700" />{@player.shields}
            </span>
```

with

```heex
            <span class="flex items-center gap-0.5" title="Military strength">
              <.military_icon class="size-3.5" />{@player.shields}
            </span>
```

and in `player_stats/1` replace the shields span with

```heex
      <span class="flex items-center gap-1" title="Military strength">
        <.military_icon class="size-5" />
        <span data-stat="military">{@player.shields}</span>
      </span>
```

(The summary toggle stays free of `data-stat` — `player_panels_test` counts them.)

`board.ex` — `#wonder-stages` moves to the top edge with the gradient flipped:

```heex
        <div
          id="wonder-stages"
          class="absolute inset-x-0 top-0 flex justify-center gap-2 bg-linear-to-b from-black/50 to-transparent p-2"
        >
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/components/game_components/icons.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/lib/helios_web/components/game_components/board.ex helios/test/helios_web/components/game_components/icons_test.exs helios/test/helios_web/components/game_components/player_panels_test.exs helios/test/helios_web/components/game_components/board_test.exs
git commit -m "feat(web): game-style military strength icon; wonder stages on top"
```

---

### Task 3: Chosen / Thinking status and the pass direction by name

**Files:**
- Modify: `helios/lib/helios_web/game_format.ex`
- Modify: `helios/lib/helios_web/components/game_components/player_panels.ex` (`status_chip/1` new; `neighbour_panel/1`, `others_strip/1` gain status)
- Modify: `helios/lib/helios_web/components/game_components/board.ex` (`my_board/1` gains status)
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex`
- Modify: `helios/lib/helios_web/live/game_live.ex` (pass statuses)
- Test: `helios/test/helios_web/game_format_test.exs`, `player_panels_test.exs`, `board_test.exs`, `top_bar_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Produces: `GameFormat.player_status(view, player_id) :: :chosen | :thinking | :playing | nil` and `GameFormat.statuses(view) :: %{String.t() => :chosen | :thinking | :playing | nil}`.
- Produces: `PlayerPanels.status_chip/1` — attrs `id` (string, required), `status` (atom or nil). Renders `<span id={@id} data-status={@status}>` or nothing when `status` is nil. Phase C's `PlayerBoard` reuses it.
- Produces: `neighbour_panel/1` and `my_board/1` attr `status`; `others_strip/1` attr `statuses` (map).

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios_web/game_format_test.exs` (inside the module; it already aliases `HeliosWeb.GameFormat` and `Helios.SampleViews` — add the aliases if missing):

```elixir
  describe "player_status/2" do
    test "choosing cards: chosen when submitted, thinking otherwise" do
      view = Helios.SampleViews.view()
      assert GameFormat.player_status(view, "1") == :chosen
      assert GameFormat.player_status(view, "2") == :thinking
      assert GameFormat.player_status(view, "9") == nil
    end

    test "extra turn: only the acting player is playing" do
      view = Helios.SampleViews.view()
      extra = %{view | phase: %{view.phase | kind: :extra_turn, extra_turn_player: "3"}}
      assert GameFormat.player_status(extra, "3") == :playing
      assert GameFormat.player_status(extra, "1") == nil
    end

    test "game over: nobody has a status" do
      view = Helios.SampleViews.view()
      over = %{view | phase: %{view.phase | kind: :game_over}}
      assert GameFormat.statuses(over) == %{"1" => nil, "2" => nil, "3" => nil, "4" => nil}
    end
  end
```

Append to `player_panels_test.exs`:

```elixir
  test "status chips say Chosen, Thinking… or Playing…" do
    for {status, label} <- [chosen: "Chosen", thinking: "Thinking…", playing: "Playing…"] do
      html = render_component(&PlayerPanels.status_chip/1, id: "status-2", status: status)
      assert attrs(html, "#status-2", "data-status") == [Atom.to_string(status)]
      assert text(html, "#status-2") == label
    end

    assert render_component(&PlayerPanels.status_chip/1, id: "status-2", status: nil) =~ ~r/^\s*$/
  end

  test "neighbour and other-player panels show their status" do
    html =
      render_component(&PlayerPanels.neighbour_panel/1,
        id: "east-panel",
        label: "East",
        player: SampleViews.player("2", %{wonder: "Rhódos", side: :b}),
        names: SampleViews.names(),
        status: :thinking
      )

    assert attrs(html, "#east-panel-details #status-2", "data-status") == ["thinking"]

    others =
      render_component(&PlayerPanels.others_strip/1,
        players: [SampleViews.player("3", %{wonder: "Éphesos"})],
        names: SampleViews.names(),
        statuses: %{"3" => :chosen}
      )

    assert attrs(others, "#player-3 #status-3", "data-status") == ["chosen"]
  end
```

Update the existing `neighbour_panel` and `others_strip` renders in `player_panels_test.exs` to pass `status: nil` / `statuses: %{}` (the new attrs are required).

In `board_test.exs`, pass `status: :chosen` to every `render_component(&Board.my_board/1, …)` call and add:

```elixir
  test "my board shows my status" do
    html = render_component(&Board.my_board/1, player: SampleViews.player("1"), status: :chosen)
    assert attrs(html, "#my-board #status-1", "data-status") == ["chosen"]
  end
```

In `top_bar_test.exs` change `assert text(html, "#pass-direction") == "Pass east"` to:

```elixir
    assert text(html, "#pass-direction") == "Pass to Bob"
```

and add:

```elixir
  test "connected players who have chosen get a check mark" do
    html = render_bar(SampleViews.view())
    assert attrs(html, "#connection-1", "data-status") == ["chosen"]
    assert attrs(html, "#connection-2", "data-status") == ["thinking"]
    assert count(html, "#connection-1 .hero-check") == 1
    assert count(html, "#connection-2 .hero-check") == 0
  end

  test "passing west names the west neighbour" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | direction: :west}})
    assert text(html, "#pass-direction") == "Pass to Dee"
  end
```

In `game_live_test.exs`, extend "three players submitting advances the turn" right after `assert has_element?(view_a, "#pending-choice")`:

```elixir
    assert has_element?(view_a, "#my-board [data-status='chosen']")
    assert has_element?(view_a, "#west-panel [data-status='thinking']")
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/game_format_test.exs test/helios_web/components/game_components test/helios_web/live/game_live_test.exs`
Expected: FAIL (`player_status/2`, `status_chip/1` undefined; text "Pass east").

- [ ] **Step 3: Implement `GameFormat`**

Add to `helios/lib/helios_web/game_format.ex`:

```elixir
  @doc "Who has chosen this turn: `:chosen`, `:thinking`, `:playing` (extra turn) or nil."
  def player_status(%{phase: %{kind: :game_over}}, _player), do: nil
  def player_status(%{phase: %{kind: :extra_turn, extra_turn_player: player}}, player), do: :playing
  def player_status(%{phase: %{kind: :extra_turn}}, _player), do: nil

  def player_status(%{submitted: submitted}, player) do
    case List.keyfind(submitted, player, 0) do
      {_player, true} -> :chosen
      {_player, false} -> :thinking
      nil -> nil
    end
  end

  def statuses(view), do: Map.new(view.players, &{&1.name, player_status(view, &1.name)})

  @doc "The neighbour who receives our hand this turn."
  def pass_target(%{phase: %{direction: :west}, west: west}), do: west
  def pass_target(%{phase: %{direction: :east}, east: east}), do: east
```

- [ ] **Step 4: Implement the chip and wire it into the panels and the board**

In `player_panels.ex` add:

```elixir
  attr :id, :string, required: true
  attr :status, :atom, default: nil

  def status_chip(assigns) do
    ~H"""
    <span
      :if={@status}
      id={@id}
      data-status={@status}
      class={[
        "inline-flex shrink-0 items-center gap-1 rounded-full px-2 py-0.5 text-xs font-semibold",
        status_class(@status)
      ]}
    >
      <.icon name={status_icon(@status)} class="size-3.5" />{status_label(@status)}
    </span>
    """
  end

  defp status_label(:chosen), do: "Chosen"
  defp status_label(:thinking), do: "Thinking…"
  defp status_label(:playing), do: "Playing…"

  defp status_icon(:chosen), do: "hero-check"
  defp status_icon(:thinking), do: "hero-ellipsis-horizontal"
  defp status_icon(:playing), do: "hero-clock"

  defp status_class(:chosen), do: "bg-emerald-100 text-emerald-800"
  defp status_class(:thinking), do: "bg-sky-100 text-sky-800"
  defp status_class(:playing), do: "bg-amber-100 text-amber-800"
```

`neighbour_panel/1`: add `attr :status, :atom, default: nil`. In the summary toggle, after the name span, add a compact, id-less marker (ids must stay unique — the details header carries the chip):

```heex
            <span
              :if={@status}
              data-status-dot={@status}
              class={["size-2 shrink-0 rounded-full", if(@status == :chosen, do: "bg-emerald-500", else: "bg-sky-400")]}
            >
            </span>
```

In the details `<header>` put the chip between label and name:

```heex
        <header class="flex items-center justify-between gap-2">
          <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
          <.status_chip id={"status-#{@player.name}"} status={@status} />
          <span class="min-w-0 truncate font-semibold text-zinc-900">
            {GameFormat.player_name(@names, @player.name)}
          </span>
        </header>
```

`others_strip/1`: add `attr :statuses, :map, default: %{}` and in each article header:

```heex
        <header class="mb-1 flex items-center justify-between gap-2">
          <span class="min-w-0 truncate font-semibold">{GameFormat.player_name(@names, player.name)}</span>
          <.status_chip id={"status-#{player.name}"} status={@statuses[player.name]} />
          <span class="text-xs text-zinc-600">
            {player.wonder} {GameFormat.side_label(player.side)}
          </span>
        </header>
```

`board.ex`: `import HeliosWeb.GameComponents.PlayerPanels, only: [player_stats: 1, status_chip: 1]`, add `attr :status, :atom, default: nil`, and as the first child of `#my-board`:

```heex
      <div class="flex items-center justify-between gap-2">
        <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">You</span>
        <.status_chip id={"status-#{@player.name}"} status={@status} />
      </div>
```

- [ ] **Step 5: Top bar**

In `top_bar.ex` add `statuses: GameFormat.statuses(assigns.view)` and `pass_to: GameFormat.player_name(assigns.names, GameFormat.pass_target(assigns.view))` to the assigns. Replace the pass-direction span:

```heex
            <span id="pass-direction" class="flex min-w-0 items-center gap-1">
              <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5 shrink-0" />
              <span class="max-w-24 truncate sm:max-w-48">
                <span class="sr-only sm:not-sr-only">Pass to </span>{@pass_to}
              </span>
            </span>
```

and give each connection `li` a status and a check:

```heex
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
```

(`text(html, "#connection-4") == "Dee"` still holds — the icon has no text.)

- [ ] **Step 6: GameLive passes the statuses**

In `game_live.ex` `render/1`:

```heex
              <.others_strip
                :if={@others != []}
                players={@others}
                names={@names}
                statuses={@statuses}
              />
              <div class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-[1fr_2fr_1fr]">
                <.neighbour_panel
                  id="west-panel"
                  label="West"
                  player={@west}
                  names={@names}
                  status={@statuses[@west.name]}
                />
                <.my_board player={@me} status={@statuses[@me.name]} />
                <.neighbour_panel
                  id="east-panel"
                  label="East"
                  player={@east}
                  names={@names}
                  status={@statuses[@east.name]}
                />
              </div>
```

and in `assign_view/2` add `statuses: GameFormat.statuses(view)` to the `assign(...)` list.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cd helios && mix test`
Expected: PASS.

- [ ] **Step 8: Precommit and commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/game_format.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/lib/helios_web/components/game_components/board.ex helios/lib/helios_web/components/game_components/top_bar.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/game_format_test.exs helios/test/helios_web/components/game_components/player_panels_test.exs helios/test/helios_web/components/game_components/board_test.exs helios/test/helios_web/components/game_components/top_bar_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "feat(web): show who has chosen; name the player we pass to"
```

---

### Task 4: Neighbour names in trades, card cost instead of "Bank", card zoom

**Files:**
- Create: `helios/lib/helios_web/components/game_components/card_zoom.ex`
- Modify: `helios/lib/helios_web/game_format.ex` (`option_label/2`, `card_cost/1`)
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`action_panel/1`, `option_group/1`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (pass `neighbours`)
- Test: `helios/test/helios_web/game_format_test.exs`, `hand_test.exs`, `helios/test/helios_web/components/game_components/card_zoom_test.exs` (new)

**Interfaces:**
- Consumes: nothing new.
- Produces: `GameFormat.option_label(option, neighbours)` where `neighbours :: %{west: String.t(), east: String.t()}` (display names); `GameFormat.card_cost(build_option) :: pos_integer() | nil`.
- Produces: `HeliosWeb.GameComponents.CardZoom.card_zoom/1` — attr `name` (card name); renders hidden `#card-zoom` with `#close-card-zoom`. `CardZoom.show_zoom/0` returns the `JS` command that opens it. Phase B's sheet and Phase C's read-only card sheet reuse both.
- Produces: `Hand.action_panel/1` attr `neighbours` (required map, as above).

- [ ] **Step 1: Write the failing tests**

In `game_format_test.exs` replace the existing `option_label` assertions with:

```elixir
  test "option labels name the neighbours and skip the bank" do
    neighbours = %{west: "Bob", east: "Cid"}
    assert GameFormat.option_label(:free, neighbours) == "Free"
    assert GameFormat.option_label({:coins, 1}, neighbours) == "Pay 1 coin"
    assert GameFormat.option_label({:coins, 3}, neighbours) == "Pay 3 coins"

    assert GameFormat.option_label(
             %{west_coins: 2, east_coins: 1, bank_coins: 1, payment: %{}},
             neighbours
           ) == "Bob 2 · Cid 1"

    assert GameFormat.option_label(
             %{west_coins: 0, east_coins: 2, bank_coins: 0, payment: %{}},
             neighbours
           ) == "Cid 2"
  end

  test "card cost is the bank part of a trade, shown once" do
    assert GameFormat.card_cost({:trade, [%{bank_coins: 1}, %{bank_coins: 1}]}) == 1
    assert GameFormat.card_cost({:trade, [%{bank_coins: 0}]}) == nil
    assert GameFormat.card_cost(:free) == nil
    assert GameFormat.card_cost({:coins, 2}) == nil
  end
```

In `hand_test.exs`, every `render_component(&Hand.action_panel/1, card: …)` gains `neighbours: %{west: "Dee", east: "Bob"}`. Change the trade assertions in "unavailable builds show the reason; trade options list who gets paid":

```elixir
    assert text(html, "#wonder-option-0") == "Dee 2"
    assert text(html, "#wonder-option-1") == "Bob 2"
    assert count(html, "#wonder-card-cost") == 0
```

and add:

```elixir
  test "a coin-cost card shows its cost once above the trade options" do
    card = %{
      name: "Tree Farm",
      category: :raw_material,
      age: 1,
      build:
        {:trade,
         [
           %{payment: %{west: [{:wood, 1}], east: []}, west_coins: 2, east_coins: 0, bank_coins: 1}
         ]},
      wonder_stage: {:unavailable, :cannot_afford},
      free_build: false
    }

    html = render_component(&Hand.action_panel/1, card: card, neighbours: %{west: "Dee", east: "Bob"})
    assert text(html, "#build-card-cost") == "+1 coin card cost"
    assert text(html, "#build-option-0") == "Dee 2"
  end

  test "the card art opens a zoom" do
    html = render_component(&Hand.action_panel/1, card: card("Tavern"), neighbours: %{west: "Dee", east: "Bob"})
    assert [js] = attrs(html, "#zoom-card", "phx-click")
    assert js =~ "#card-zoom"
    assert attrs(html, "#card-zoom img", "src") == ["/images/cards/tavern.png"]
  end
```

Create `helios/test/helios_web/components/game_components/card_zoom_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.CardZoomTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardZoom

  test "the zoom starts hidden and closes on click, Escape and the close button" do
    html = render_component(&CardZoom.card_zoom/1, name: "Altar")
    assert [class] = attrs(html, "#card-zoom", "class")
    assert class =~ "hidden"
    assert attrs(html, "#card-zoom", "role") == ["dialog"]
    assert attrs(html, "#card-zoom", "phx-key") == ["Escape"]
    assert [click] = attrs(html, "#card-zoom", "phx-click")
    assert click =~ "hide"
    assert attrs(html, "#close-card-zoom", "aria-label") == ["Close"]
    assert attrs(html, "#card-zoom img", "alt") == ["Altar"]
  end
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/game_format_test.exs test/helios_web/components/game_components`
Expected: FAIL (`option_label/2`, `card_cost/1`, `CardZoom` undefined).

- [ ] **Step 3: Implement `GameFormat`**

Replace the three `option_label/1` clauses in `game_format.ex` with:

```elixir
  def option_label(:free, _neighbours), do: "Free"
  def option_label({:coins, 1}, _neighbours), do: "Pay 1 coin"
  def option_label({:coins, n}, _neighbours), do: "Pay #{n} coins"

  def option_label(%{west_coins: west, east_coins: east}, %{west: west_name, east: east_name}) do
    [{west_name, west}, {east_name, east}]
    |> Enum.reject(fn {_name, coins} -> coins == 0 end)
    |> Enum.map_join(" · ", fn {name, coins} -> "#{name} #{coins}" end)
  end

  @doc "The card's own coin cost when it is paid on top of a trade; nil otherwise."
  def card_cost({:trade, [%{bank_coins: coins} | _]}) when coins > 0, do: coins
  def card_cost(_option), do: nil
```

- [ ] **Step 4: Create the zoom component**

`helios/lib/helios_web/components/game_components/card_zoom.ex`:

```elixir
defmodule HeliosWeb.GameComponents.CardZoom do
  @moduledoc "Full-size card art over a dark scrim. Client-side only (JS show/hide)."
  use HeliosWeb, :html

  alias HeliosWeb.GameAssets

  @doc "Opens the zoom rendered by `card_zoom/1`."
  def show_zoom do
    JS.show(to: "#card-zoom", display: "flex") |> JS.focus(to: "#close-card-zoom")
  end

  attr :name, :string, required: true

  def card_zoom(assigns) do
    ~H"""
    <div
      id="card-zoom"
      role="dialog"
      aria-modal="true"
      aria-label={@name}
      phx-click={JS.hide(to: "#card-zoom")}
      phx-window-keydown={JS.hide(to: "#card-zoom")}
      phx-key="Escape"
      class="fixed inset-0 z-[60] hidden items-center justify-center bg-black/80 p-4"
    >
      <img
        src={GameAssets.card_path(@name)}
        alt={@name}
        class="max-h-[min(440px,85dvh)] w-auto rounded-xl shadow-2xl"
      />
      <button
        id="close-card-zoom"
        type="button"
        aria-label="Close"
        class="absolute right-4 top-4 inline-flex size-10 items-center justify-center rounded-full bg-white/15 text-white transition hover:bg-white/30 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-6" />
      </button>
    </div>
    """
  end
end
```

(`JS` is already aliased for every `use HeliosWeb, :html` module by `html_helpers/0` in `helios_web.ex`.)

- [ ] **Step 5: Use it in the action sheet, pass neighbours, show the card cost**

In `hand.ex`:
- `import HeliosWeb.GameComponents.CardZoom, only: [card_zoom: 1, show_zoom: 0]`.
- `action_panel/1`: add `attr :neighbours, :map, required: true`; pass `neighbours={@neighbours}` to both `option_group` calls; replace the `<img …>` of the card with

```heex
        <button
          id="zoom-card"
          type="button"
          phx-click={show_zoom()}
          aria-label={"Enlarge #{@card.name}"}
          class="relative shrink-0 cursor-zoom-in rounded-lg focus:outline-none focus-visible:ring-4 focus-visible:ring-sky-300"
        >
          <img
            src={GameAssets.card_path(@card.name)}
            alt={@card.name}
            class="h-[147px] w-24 rounded-lg sm:h-[183px] sm:w-[120px] short:h-[98px] short:w-16"
          />
          <span class="absolute bottom-1 right-1 rounded-full bg-black/60 p-1 text-white">
            <.icon name="hero-magnifying-glass-plus" class="size-4" />
          </span>
        </button>
```

  and render `<.card_zoom name={@card.name} />` as the last child of `#action-panel`.
- `option_group/1`: add `attr :neighbours, :map, required: true`; in the `{:trade, options}` branch, above the buttons:

```heex
          <p
            :if={GameFormat.card_cost(@option)}
            id={"#{@prefix}-card-cost"}
            class="text-xs text-zinc-600"
          >
            +{GameFormat.card_cost(@option)} {if GameFormat.card_cost(@option) == 1, do: "coin", else: "coins"} card cost
          </p>
```

  and change both label calls to `GameFormat.option_label(payment_option, @neighbours)` / `GameFormat.option_label(simple, @neighbours)`.

In `game_live.ex`:

```heex
            <.action_panel
              :if={@selected_card}
              card={@selected_card}
              neighbours={%{
                west: GameFormat.player_name(@names, @view.west),
                east: GameFormat.player_name(@names, @view.east)
              }}
            />
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cd helios && mix test`
Expected: PASS.

- [ ] **Step 7: Phase gate — precommit, e2e, commit**

```bash
cd helios && mix precommit
cd ../e2e && npm ci && npx playwright install chromium && npx playwright test
```

Expected: all green. `responsive.spec.ts` checks every visible button in the action sheet is in the viewport and ≥ 44px on touch: `#zoom-card` is the 96px-wide art, so it passes; if the top bar exceeds 96px on phones because of the pass-to name, tighten `max-w-24` to `max-w-16` and re-run.

```bash
git add helios/lib/helios_web/components/game_components/card_zoom.ex helios/lib/helios_web/game_format.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/game_format_test.exs helios/test/helios_web/components/game_components/hand_test.exs helios/test/helios_web/components/game_components/card_zoom_test.exs
git commit -m "feat(web): name neighbours in trades, show card cost once, zoom card art"
```

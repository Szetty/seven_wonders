defmodule HeliosWeb.GameLive do
  @moduledoc """
  The game table. The page never holds engine state of its own: it re-fetches
  the player's view on every `{:game_updated, seq}` and sends choices as
  `{card, kind, option index}` and setup claims as `{wonder, side}`; the GameServer
  resolves them against its view. While `view.phase.kind == :setup` the page shows
  `WonderSetup` instead of the table.
  """
  use HeliosWeb, :live_view

  require Logger

  import HeliosWeb.GameComponents.AgeSummary
  import HeliosWeb.GameComponents.CardSheet
  import HeliosWeb.GameComponents.ExtraTurn
  import HeliosWeb.GameComponents.Fx
  import HeliosWeb.GameComponents.Hand
  import HeliosWeb.GameComponents.HistoryDrawer
  import HeliosWeb.GameComponents.PlayerBoard
  import HeliosWeb.GameComponents.ReplayBar
  import HeliosWeb.GameComponents.Scoreboard
  import HeliosWeb.GameComponents.TopBar
  import HeliosWeb.GameComponents.WonderSetup

  alias Helios.{Catalogue, Games, Lobbies}
  alias Helios.Games.Choice
  alias HeliosWeb.GameComponents.ExtraTurn
  alias HeliosWeb.{GameFormat, Presence, TableLayout, TurnFx}

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
      <div id="game" class="flex min-h-dvh flex-col">
        <.fx_layer />
        <.history_drawer
          history={@history}
          ctx={%{me: @view.me, names: @names, view: @view, random: @random}}
          frames={if @replay, do: frame_links(@replay), else: %{}}
        />
        <.age_summary
          :if={@age_summary}
          event={@age_summary}
          ctx={%{me: @view.me, names: @names, view: @view}}
        />
        <div class="mx-auto flex w-full max-w-[96rem] flex-1 flex-col gap-3 px-2 pt-3 sm:gap-4 sm:px-4 sm:pt-4 lg:px-8">
          <.top_bar
            view={
              if @replay,
                do: %{
                  @view
                  | phase: current_frame(@replay).phase,
                    submitted: [],
                    discard_count: current_frame(@replay).discard_count
                },
                else: @view
            }
            names={@names}
            connected={@connected}
            ai_ids={@ai_ids}
            replay={@replay != nil}
          />
          <%= if @view.phase.kind == :setup do %>
            <.wonder_setup
              setup={@view.setup}
              me={@setup_as}
              viewer={@view.me}
              acting={@setup_acting}
              ai_ids={@ai_ids}
              names={@names}
              wonders={@wonder_names}
              previews={@setup_previews}
            />
          <% else %>
            <%= if @view.phase.kind == :game_over do %>
              <div class="flex flex-col gap-3 pb-4 sm:gap-4">
                <.replay_bar
                  :if={@replay}
                  frame={current_frame(@replay)}
                  index={@replay.index}
                  last={length(@replay.frames) - 1}
                  playing={@replay.playing}
                />
                <%= if is_nil(@replay) or @replay.index == length(@replay.frames) - 1 or @show_scores do %>
                  <.scoreboard
                    scores={@view.scores}
                    names={@names}
                    me={@view.me}
                    lobby_id={@game.lobby_id}
                  />
                <% else %>
                  <button
                    id="show-final-scores"
                    type="button"
                    phx-click="toggle_final_scores"
                    class="mx-auto rounded-lg bg-white/80 px-3 py-1.5 text-sm font-semibold text-zinc-900 ring-1 ring-zinc-300 hover:bg-white pointer-coarse:min-h-11"
                  >
                    Final scores
                  </button>
                <% end %>
                <p
                  :if={!@boards_available}
                  id="boards-unavailable"
                  class="mx-auto rounded-lg bg-antique/90 px-3 py-2 text-sm text-zinc-700"
                >
                  Boards are unavailable for this game
                </p>
                <.table_area
                  :if={@boards_available}
                  layout={@table_layout}
                  me_id={@view.me}
                  names={@names}
                  statuses={@statuses}
                  ai_ids={@ai_ids}
                  random_now={
                    if @replay, do: MapSet.new(), else: GameFormat.last_random(@history, @random)
                  }
                />
                <div
                  :if={@replay && (current_frame(@replay).hand != [] or frame_discard(@replay) != [])}
                  class="sticky bottom-0 z-30 overflow-x-clip rounded-t-xl bg-antique/95 p-2 shadow"
                >
                  <.replay_discard
                    :if={frame_discard(@replay) != []}
                    cards={frame_discard(@replay)}
                    chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                  />
                  <.replay_hand
                    :if={current_frame(@replay).hand != []}
                    hand={current_frame(@replay).hand}
                    chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                  />
                </div>
                <.card_sheet
                  :if={@info_card}
                  name={@info_card.name}
                  neighbours={GameFormat.neighbours_of(@view, @info_card.owner, @names)}
                  built={Enum.map(@me.built, & &1.name)}
                />
              </div>
            <% else %>
              <.table_area
                layout={@table_layout}
                me_id={@view.me}
                names={@names}
                statuses={@statuses}
                ai_ids={@ai_ids}
                random_now={
                  if @replay, do: MapSet.new(), else: GameFormat.last_random(@history, @random)
                }
              />
              <div
                :if={GameFormat.dock?(@view)}
                id="dock"
                class="sticky bottom-0 z-30 -mx-2 flex flex-col gap-2 overflow-x-clip rounded-t-xl bg-antique/95 px-2 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] shadow-[0_-4px_12px_rgba(0,0,0,0.2)] sm:mx-0 sm:px-3"
              >
                <.extra_turn_notice view={@view} names={@names} />
                <.discard_restore view={@view} minimised={@discard_minimised} />
                <.pending_choice
                  :if={@view.my_pending && is_nil(@selected)}
                  action={@view.my_pending}
                />
                <.hand
                  :if={GameFormat.show_hand?(@view)}
                  hand={@view.hand}
                  selected={@selected}
                  pending={@view.my_pending}
                />
              </div>
              <.discard_picker view={@view} minimised={@discard_minimised} />
              <.action_panel
                :if={@selected_card}
                card={@selected_card}
                neighbours={
                  %{
                    west: GameFormat.player_name(@names, @view.west),
                    east: GameFormat.player_name(@names, @view.east)
                  }
                }
                built={Enum.map(@me.built, & &1.name)}
                next_stage={if @me.stages_built < @me.stages_total, do: @me.stages_built + 1}
              />
              <.card_sheet
                :if={@info_card}
                name={@info_card.name}
                neighbours={GameFormat.neighbours_of(@view, @info_card.owner, @names)}
                built={Enum.map(@me.built, & &1.name)}
              />
            <% end %>
          <% end %>
        </div>
      </div>
    </Layouts.app>
    """
  end

  attr :layout, :map, required: true
  attr :me_id, :string, required: true
  attr :names, :map, required: true
  attr :statuses, :map, required: true
  attr :ai_ids, :any, default: MapSet.new()
  attr :random_now, :any, default: MapSet.new()

  defp table_area(assigns) do
    ~H"""
    <div id="table" class="flex flex-1 flex-col gap-3 pb-1 sm:gap-4">
      <div
        id="seats"
        phx-hook=".BoardPanels"
        class="mx-auto flex max-w-2xl flex-col gap-3 sm:gap-4 xl:relative xl:block xl:max-w-none xl:min-h-(--ring-height)"
        style={"--ring-height: #{@layout.ring_height}rem; --me-built-max: #{TableLayout.me_built_max()}rem"}
      >
        <svg
          id="table-ring"
          class="hidden xl:block absolute inset-x-0 top-0 pointer-events-none"
          style={"height: #{@layout.ring_height}rem"}
          viewBox={"0 0 100 #{@layout.ring_height}"}
          preserveAspectRatio="none"
          aria-hidden="true"
        >
          <path
            d={@layout.ring_path}
            fill="none"
            stroke="currentColor"
            stroke-width="0.25"
            class="text-stone-400/50"
          />
        </svg>
        <.player_board
          :for={seat <- @layout.seats}
          id={board_dom_id(seat)}
          label={TableLayout.label(seat)}
          player={seat.player}
          names={@names}
          status={@statuses[seat.player.name]}
          ai={MapSet.member?(@ai_ids, seat.player.name)}
          random={MapSet.member?(@random_now, seat.player.name)}
          mine={seat.role == :me}
          neighbours={TableLayout.neighbours(seat, @names, @me_id)}
          panel_align={seat.panel_align}
          style={board_style(seat)}
          class={board_class(seat)}
        />
      </div>
      <script :type={Phoenix.LiveView.ColocatedHook} name=".BoardPanels">
        export default {
          mounted() {
            this.boundClick = (e) => this.documentClick(e)
            this.boundKey = (e) => this.keydown(e)
            document.addEventListener("click", this.boundClick)
            document.addEventListener("keydown", this.boundKey)
          },
          destroyed() {
            document.removeEventListener("click", this.boundClick)
            document.removeEventListener("keydown", this.boundKey)
          },
          wide() {
            return window.matchMedia("(min-width: 80rem)").matches
          },
          openSections() {
            return [...this.el.querySelectorAll("section.is-open")]
          },
          sectionOf(target) {
            return target && target.closest ? target.closest("section[data-player]") : null
          },
          open(section) {
            if (this.wide()) this.closeOthers(section)
            this.js().addClass(section, "is-open")
            const btn = section.querySelector("[data-board-toggle]")
            if (btn) this.js().setAttribute(btn, "aria-expanded", "true")
          },
          close(section) {
            this.js().removeClass(section, "is-open")
            const btn = section.querySelector("[data-board-toggle]")
            if (btn) this.js().setAttribute(btn, "aria-expanded", "false")
          },
          closeOthers(except) {
            for (const section of this.openSections()) {
              if (section !== except) this.close(section)
            }
          },
          closeAll() {
            for (const section of this.openSections()) this.close(section)
          },
          documentClick(e) {
            const toggle = e.target.closest && e.target.closest("[data-board-toggle]")
            if (toggle) {
              const section = this.sectionOf(toggle)
              if (section && this.el.contains(section)) {
                if (section.classList.contains("is-open")) this.close(section)
                else this.open(section)
                return
              }
            }
            if (!this.wide()) return
            const open = this.openSections()
            if (open.length === 0) return
            if (e.target.closest && e.target.closest("#card-sheet, #card-sheet-scrim")) return
            if (open.some((s) => s.contains(e.target))) return
            this.closeAll()
          },
          keydown(e) {
            if (e.key !== "Escape") return
            if (document.querySelector("#card-sheet") || document.querySelector("#action-panel")) return
            this.closeAll()
          },
        }
      </script>
    </div>
    """
  end

  defp board_dom_id(%{role: :me}), do: "my-board"
  defp board_dom_id(%{role: :west}), do: "west-panel"
  defp board_dom_id(%{role: :east}), do: "east-panel"
  defp board_dom_id(%{player: player}), do: "board-#{player.name}"

  defp board_class(%{role: :me}), do: "xl:relative xl:z-10 xl:ml-[30%] xl:w-[40%]"

  defp board_class(_seat),
    do: "xl:absolute xl:z-10 xl:left-(--ring-left) xl:top-(--ring-top) xl:w-(--ring-width)"

  defp board_style(%{role: :me}), do: nil

  defp board_style(%{ring: ring}),
    do: "--ring-left: #{ring.left}%; --ring-top: #{ring.top}rem; --ring-width: #{ring.width}%"

  @impl true
  def mount(%{"game_id" => game_id}, _session, socket) do
    case Games.fetch_game(game_id) do
      {:ok, game} -> mount_game(socket, game, socket.assigns.current_scope.user)
      {:error, reason} -> {:ok, leave(socket, reason, nil)}
    end
  end

  # The replay frame lives in the URL (?frame=N) so a reload returns to it.
  # Active games and games without boards have no replay: params are ignored.
  @impl true
  def handle_params(params, _uri, %{assigns: %{replay: %{} = replay}} = socket) do
    index = frame_param(params["frame"], length(replay.frames) - 1)
    {:noreply, if(index == replay.index, do: socket, else: goto(socket, index))}
  end

  def handle_params(_params, _uri, socket), do: {:noreply, socket}

  defp mount_game(socket, game, user) do
    case authorize(game, user) do
      :ok -> mount_authorized(socket, game, user)
      {:error, :unauthorized} -> {:ok, leave(socket, :unauthorized, nil)}
    end
  end

  defp mount_authorized(socket, game, user) do
    arrive(socket, game)

    if game.status == "finished" do
      mount_finished(socket, game, user)
    else
      mount_active(socket, game, user)
    end
  end

  # A seated player opening a game straight from a table they were invited to
  # "arrives" there too; the `{:invite_resolved, _}` broadcast clears the banner.
  defp arrive(socket, game) do
    if connected?(socket),
      do: Lobbies.accept_pending(socket.assigns.current_scope, game.lobby_id)

    :ok
  end

  defp mount_active(socket, game, user) do
    case Games.snapshot(game.id, user.id) do
      {:ok, %{view: view, history: history, random: random}} ->
        {:ok,
         socket
         |> setup(game, user)
         |> assign_view(view)
         |> assign(:history, history)
         |> assign(:random, random)}

      {:error, reason} ->
        {:ok, leave(socket, reason, game.lobby_id)}
    end
  end

  # A finished game is read-only: the GameServer is never started for it (which
  # would raise `:not_active`). The final boards come from replaying the action
  # log in a throwaway engine; if the log no longer replays, the table falls
  # back to the scores the finish path persisted on the `games` row.
  defp mount_finished(socket, game, user) do
    socket = setup(socket, game, user)

    case Games.replay(game, user.id) do
      {:ok, replay} ->
        {:ok, socket |> assign(:boards_available, true) |> enter_replay(replay)}

      {:error, reason} ->
        Logger.warning("Game #{game.id} boards unavailable: #{inspect(reason)}")

        {:ok,
         socket
         |> assign(:boards_available, false)
         |> assign(:history, [])
         |> assign_finished_view(game, user)}
    end
  end

  defp enter_replay(socket, %{view: view, history: history, frames: frames, random: random}) do
    socket
    |> assign(history: history, show_scores: false, random: random)
    |> assign(:replay, %{frames: frames, index: length(frames) - 1, playing: false})
    |> assign_view(view)
    |> assign_frame()
  end

  defp current_frame(%{frames: frames, index: index}), do: Enum.at(frames, index)

  # Only the viewer's own build-from-discard frame has a pile (Games.replay/2).
  defp frame_discard(replay), do: current_frame(replay).discard_pile || []

  # The table shows the frame's public boards; @view keeps the final view (scores, me, neighbours).
  defp assign_frame(socket) do
    frame = current_frame(socket.assigns.replay)
    by_name = Map.new(frame.players, &{&1.name, &1})

    assign(socket,
      me: Map.fetch!(by_name, socket.assigns.view.me),
      table_layout: TableLayout.build(frame.players, socket.assigns.view.me),
      statuses: %{}
    )
  end

  defp frame_links(%{frames: frames}) do
    for {%{event_index: index}, frame} <- Enum.with_index(frames),
        index != nil,
        into: %{},
        do: {index, frame}
  end

  # Missing or non-integer → the final frame; integers are clamped to 0..last.
  defp frame_param(value, last) when is_binary(value) do
    case Integer.parse(value) do
      {index, ""} -> index |> max(0) |> min(last)
      _ -> last
    end
  end

  defp frame_param(_value, last), do: last

  # The one place a replay frame change is announced: handle_params/3 then
  # shows the frame. The final frame keeps the plain game URL.
  defp patch_frame(socket, index) do
    %{game: game, replay: replay} = socket.assigns

    to =
      if index == length(replay.frames) - 1,
        do: ~p"/game/#{game.id}",
        else: ~p"/game/#{game.id}?frame=#{index}"

    push_patch(socket, to: to, replace: true)
  end

  defp pause(socket), do: update(socket, :replay, &%{&1 | playing: false})

  # History-drawer links: only real frame numbers move; anything else is ignored.
  defp goto_param(socket, frame, last) do
    case Integer.parse(to_string(frame)) do
      {index, ""} when index >= 0 and index <= last -> socket |> pause() |> patch_frame(index)
      _ -> socket
    end
  end

  defp goto(socket, index) do
    socket
    |> update(:replay, &%{&1 | index: index, playing: false})
    |> assign(age_summary: nil)
    |> assign_frame()
  end

  defp step_forward(socket) do
    %{replay: replay, history: history, view: view} = socket.assigns
    frame = current_frame(replay)
    turn = Enum.at(history, frame.event_index)
    next = Enum.at(replay.frames, replay.index + 1)
    pass = if next.phase.kind == :choosing_cards and next.phase.turn > 1, do: next.phase.direction

    socket =
      push_event(
        socket,
        "turn_resolved",
        TurnFx.payload(turn, %{
          me: view.me,
          my_card: GameFormat.action_card(frame.my_choice),
          pass: pass
        })
      )

    # Stepping into a frame that reveals the end-of-age conflicts pauses
    # autoplay (brief step 6: playback stops at the summary row; Play resumes).
    {revealed?, socket} =
      case Enum.at(history, frame.event_index + 1) do
        {:battles, _} = battles -> {true, assign(socket, :age_summary, battles)}
        _ -> {false, socket}
      end

    last = length(replay.frames) - 1

    socket
    |> update(:replay, fn replay ->
      %{
        replay
        | index: replay.index + 1,
          playing: replay.playing and replay.index + 1 < last and not revealed?
      }
    end)
    |> assign_frame()
    |> then(&patch_frame(&1, &1.assigns.replay.index))
  end

  defp setup(socket, game, user) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
      {:ok, _ref} = Presence.track(self(), Games.topic(game.id), user.id, %{name: user.name})
    end

    players = Games.players(game)

    owner? =
      case Lobbies.fetch_lobby(game.lobby_id) do
        {:ok, lobby} -> lobby.owner_id == user.id
        {:error, _reason} -> false
      end

    socket
    |> assign(
      page_title: "7 Wonders",
      game: game,
      names: Games.display_names(players),
      ai_ids: Games.ai_player_ids(players),
      owner?: owner?,
      setup_as: nil,
      setup_acting: [],
      random: MapSet.new(),
      info_card: nil,
      discard_minimised: false,
      boards_available: true,
      age_summary: nil,
      last_choice: nil,
      replay: nil,
      show_scores: false,
      wonder_names: Catalogue.wonder_names(),
      setup_previews: %{}
    )
    |> assign_connected()
  end

  # A view shaped like the engine's game-over view, built without booting it:
  # `final_scores` supplies the scores, `players` only the names the top bar
  # needs, and the fixed end state supplies the phase (game over only happens
  # in age III, after the sixth turn discards every last card).
  defp assign_finished_view(socket, game, user) do
    ids = Enum.map(Games.players(game), &Games.player_id/1)
    me = to_string(user.id)
    index = Enum.find_index(ids, &(&1 == me)) || 0
    count = length(ids)

    view = %{
      me: me,
      west: Enum.at(ids, rem(index - 1 + count, count)),
      east: Enum.at(ids, rem(index + 1, count)),
      players: Enum.map(ids, &%{name: &1}),
      phase: %{
        kind: :game_over,
        age: 3,
        turn: 6,
        direction: :west,
        extra_turn_player: nil,
        extra_turn_kind: nil
      },
      hand: [],
      discard_pile: nil,
      discard_count: 0,
      submitted: [],
      my_pending: nil,
      scores: persisted_scores(game)
    }

    assign_view(socket, view)
  end

  @score_keys [
    :player,
    :military,
    :treasury,
    :wonder,
    :civilian,
    :scientific,
    :commercial,
    :guild,
    :total,
    :coins,
    :rank
  ]

  # `GameServer` persists scores with string keys (JSON round-trip); the
  # scoreboard reads the atom-keyed shape the engine view uses.
  defp persisted_scores(%{final_scores: %{"scores" => scores}}) when is_list(scores) do
    Enum.map(scores, fn score ->
      Map.new(@score_keys, fn key -> {key, Map.fetch!(score, Atom.to_string(key))} end)
    end)
  end

  defp persisted_scores(_game), do: []

  @impl true
  def handle_event("replay_" <> action, params, %{assigns: %{replay: %{} = replay}} = socket) do
    last = length(replay.frames) - 1

    socket =
      case {action, params} do
        {"first", _} ->
          socket |> pause() |> patch_frame(0)

        {"prev", _} ->
          socket |> pause() |> patch_frame(max(replay.index - 1, 0))

        {"last", _} ->
          socket |> pause() |> patch_frame(last)

        {"next", _} when replay.index < last ->
          step_forward(socket)

        {"next", _} ->
          update(socket, :replay, &%{&1 | playing: false})

        {"toggle", _} ->
          update(socket, :replay, &%{&1 | playing: not &1.playing and &1.index < last})

        {"goto", %{"frame" => frame}} ->
          goto_param(socket, frame, last)

        _ ->
          socket
      end

    {:noreply, socket}
  end

  def handle_event("replay_" <> _, _params, socket), do: {:noreply, socket}

  def handle_event("toggle_final_scores", _params, socket),
    do: {:noreply, update(socket, :show_scores, &(!&1))}

  def handle_event("select_card", %{"card" => card}, socket) do
    name = if socket.assigns.selected == card, do: nil, else: card
    {:noreply, assign_selection(socket, name)}
  end

  # `phx-key="Escape"` filters keys in the browser; this guard keeps the server
  # safe from other keys (and hand-crafted events) too.
  def handle_event("deselect", %{"key" => key}, socket) when key != "Escape",
    do: {:noreply, socket}

  def handle_event("deselect", _params, socket), do: {:noreply, assign_selection(socket, nil)}

  # Only cards actually on the table can be opened — a forged event names nothing.
  # Finished views carry players without :built, so guard the lookup.
  def handle_event("show_card", %{"card" => card, "owner" => owner}, socket)
      when is_binary(card) and is_binary(owner) do
    known? =
      Enum.any?(socket.assigns.view.players, fn player ->
        player.name == owner and Enum.any?(Map.get(player, :built, []), &(&1.name == card))
      end)

    info = if known?, do: %{name: card, owner: owner}
    {:noreply, socket |> assign_selection(nil) |> assign(:info_card, info)}
  end

  def handle_event("hide_card", %{"key" => key}, socket) when key != "Escape",
    do: {:noreply, socket}

  def handle_event("hide_card", _params, socket), do: {:noreply, assign(socket, :info_card, nil)}

  def handle_event("minimise_discard", _params, socket),
    do: {:noreply, assign(socket, :discard_minimised, true)}

  def handle_event("dismiss_age_summary", _params, socket),
    do: {:noreply, assign(socket, :age_summary, nil)}

  def handle_event("restore_discard", _params, socket),
    do: {:noreply, assign(socket, :discard_minimised, false)}

  def handle_event("submit", params, socket) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    result =
      with {:ok, choice} <- Choice.parse(params),
           :ok <- Games.submit_choice(game.id, user.id, choice) do
        {:ok, choice}
      end

    socket =
      case result do
        {:ok, choice} ->
          socket |> assign(:last_choice, choice.card) |> assign_selection(nil)

        {:error, reason} ->
          put_flash(socket, :error, Games.error_message(reason))
      end

    {:noreply, refresh(socket)}
  end

  def handle_event("claim_wonder", params, socket),
    do: submit_setup(socket, Choice.parse_claim(params))

  def handle_event("confirm_wonder", _params, socket),
    do: submit_setup(socket, {:ok, :confirm_wonder})

  # Only seats in @setup_acting (the viewer, plus AI seats for the owner) can be chosen.
  def handle_event("setup_as", %{"seat" => seat}, socket) do
    if Enum.any?(socket.assigns.setup_acting, &(&1.id == seat)),
      do: {:noreply, assign(socket, :setup_as, seat)},
      else: {:noreply, socket}
  end

  def handle_event("setup_as", _params, socket), do: {:noreply, socket}

  # Which side a free board shows before it is claimed: local to this page.
  def handle_event("show_side", %{"wonder" => wonder, "side" => side}, socket)
      when side in ["a", "b"] do
    previews =
      if wonder in socket.assigns.wonder_names,
        do: Map.put(socket.assigns.setup_previews, wonder, if(side == "a", do: :a, else: :b)),
        else: socket.assigns.setup_previews

    {:noreply, assign(socket, :setup_previews, previews)}
  end

  def handle_event("show_side", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_info({:game_updated, _seq}, socket), do: {:noreply, refresh(socket)}
  def handle_info({:game_finished}, socket), do: {:noreply, refresh(socket)}

  def handle_info({:game_aborted}, socket) do
    {:noreply, leave(socket, :aborted, socket.assigns.game.lobby_id)}
  end

  def handle_info(%Phoenix.Socket.Broadcast{event: "presence_diff"}, socket) do
    {:noreply, assign_connected(socket)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp authorize(game, user) do
    if Games.seated?(game, user.id), do: :ok, else: {:error, :unauthorized}
  end

  # Claims and confirmations are ordinary engine actions; the engine decides.
  # A lost race (`:wonder_taken`) names the board that was just taken.
  defp submit_setup(socket, parsed) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    result =
      with {:ok, action} <- parsed do
        if socket.assigns.setup_as in [nil, socket.assigns.view.me] do
          Games.submit(game.id, user.id, action)
        else
          {:ai, seat} = Helios.Games.Actor.from_player_id(socket.assigns.setup_as)
          Games.submit_for_ai_seat(socket.assigns.current_scope, game.id, seat, action)
        end
      end

    socket =
      case result do
        :ok ->
          clear_flash(socket, :error)

        {:error, :wonder_taken} ->
          {:ok, {:claim_wonder, %{wonder: wonder}}} = parsed
          put_flash(socket, :error, "#{wonder} was just taken")

        {:error, reason} ->
          put_flash(socket, :error, Games.error_message(reason))
      end

    {:noreply, refresh(socket)}
  end

  defp refresh(socket) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    case Games.snapshot(game.id, user.id) do
      {:ok, %{view: view, history: history, random: random}} ->
        socket =
          socket
          |> animate_new_events(history, view)
          |> assign(:history, history)
          |> assign(:random, random)
          |> assign(:last_choice, GameFormat.action_card(view.my_pending))
          |> assign_view(view)

        if view.phase.kind == :game_over and is_nil(socket.assigns[:replay]) do
          case Games.replay(game, user.id) do
            {:ok, replay} -> enter_replay(socket, replay)
            {:error, _reason} -> socket
          end
        else
          # In replay, assign_view rewrote the boards from the final view;
          # re-apply the current frame so refresh is idempotent.
          if socket.assigns[:replay], do: assign_frame(socket), else: socket
        end

      {:error, reason} ->
        leave(socket, reason, game.lobby_id)
    end
  end

  defp animate_new_events(socket, history, view) do
    new = Enum.drop(history, length(socket.assigns.history))
    pass = if view.phase.kind == :choosing_cards and view.phase.turn > 1, do: view.phase.direction

    Enum.reduce(new, socket, fn
      {:turn, _} = event, socket ->
        push_event(
          socket,
          "turn_resolved",
          TurnFx.payload(event, %{me: view.me, my_card: socket.assigns.last_choice, pass: pass})
        )

      {:battles, _} = event, socket ->
        assign(socket, :age_summary, event)
    end)
  end

  # During setup the engine view has no boards and no hand (`players: []`):
  # nothing is derived from players, and the table, dock and sheets are not
  # rendered. Selection and the card sheet are cleared so no stale panel
  # survives into Age I.
  defp assign_view(socket, %{phase: %{kind: :setup}} = view) do
    acting =
      [%{id: view.me, name: "You"}] ++
        if socket.assigns.owner?,
          do:
            for(
              seat <- view.setup.seats,
              MapSet.member?(socket.assigns.ai_ids, seat.player),
              do: %{
                id: seat.player,
                name: GameFormat.player_name(socket.assigns.names, seat.player)
              }
            ),
          else: []

    setup_as =
      if Enum.any?(acting, &(&1.id == socket.assigns[:setup_as])),
        do: socket.assigns.setup_as,
        else: view.me

    assign(socket,
      view: view,
      me: nil,
      table_layout: nil,
      statuses: GameFormat.statuses(view),
      selected: nil,
      selected_card: nil,
      info_card: nil,
      discard_minimised: false,
      setup_acting: acting,
      setup_as: setup_as
    )
  end

  defp assign_view(socket, view) do
    by_name = Map.new(view.players, &{&1.name, &1})

    socket
    |> assign(
      view: view,
      me: Map.fetch!(by_name, view.me),
      table_layout: TableLayout.build(view.players, view.me),
      statuses: GameFormat.statuses(view)
    )
    |> assign_selection(socket.assigns[:selected])
    |> assign(
      :discard_minimised,
      socket.assigns[:discard_minimised] == true and ExtraTurn.my_discard_turn?(view)
    )
  end

  defp assign_selection(socket, name) do
    card = name && Enum.find(socket.assigns.view.hand, &(&1.name == name))
    assign(socket, selected: card && card.name, selected_card: card)
  end

  # AI seats have no browser: they always count as connected.
  defp assign_connected(socket) do
    humans = Games.connected_user_ids(Games.topic(socket.assigns.game.id))
    assign(socket, :connected, MapSet.union(humans, socket.assigns.ai_ids))
  end

  defp leave(socket, reason, lobby_id) do
    path =
      if lobby_id do
        ~p"/lobby/#{lobby_id}"
      else
        ~p"/lobby/#{Lobbies.get_or_create_own_lobby(socket.assigns.current_scope.user).id}"
      end

    socket
    |> put_flash(:error, Games.error_message(reason))
    |> push_navigate(to: path)
  end
end

defmodule HeliosWeb.GameLive do
  @moduledoc """
  The game table. The page never holds engine state of its own: it re-fetches
  the player's view on every `{:game_updated, seq}` and sends choices as
  `{card, kind, option index}`; the GameServer resolves them against its view.
  """
  use HeliosWeb, :live_view

  require Logger

  import HeliosWeb.GameComponents.CardSheet
  import HeliosWeb.GameComponents.ExtraTurn
  import HeliosWeb.GameComponents.Fx
  import HeliosWeb.GameComponents.Hand
  import HeliosWeb.GameComponents.HistoryDrawer
  import HeliosWeb.GameComponents.PlayerBoard
  import HeliosWeb.GameComponents.Scoreboard
  import HeliosWeb.GameComponents.TopBar

  alias Helios.{Games, Lobbies}
  alias Helios.Games.Choice
  alias HeliosWeb.GameComponents.ExtraTurn
  alias HeliosWeb.{GameFormat, Presence, TurnFx}

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app
      flash={@flash}
      current_scope={@current_scope}
      notifications={@notifications}
      background="bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed"
    >
      <div id="game" class="flex min-h-dvh flex-col">
        <.fx_layer />
        <.history_drawer
          history={@history}
          ctx={%{me: @view.me, names: @names, view: @view}}
        />
        <div class="mx-auto flex w-full max-w-7xl flex-1 flex-col gap-3 px-2 pt-3 sm:gap-4 sm:px-4 sm:pt-4 lg:px-8">
          <.top_bar view={@view} names={@names} connected={@connected} />
          <%= if @view.phase.kind == :game_over do %>
            <div class="flex flex-col gap-3 pb-4 sm:gap-4">
              <.scoreboard
                scores={@view.scores}
                names={@names}
                me={@view.me}
                lobby_id={@game.lobby_id}
              />
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
            <.table_area
              me={@me}
              west={@west}
              east={@east}
              others={@others}
              names={@names}
              statuses={@statuses}
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
        </div>
      </div>
    </Layouts.app>
    """
  end

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
        <.player_board
          id="west-panel"
          label="West"
          player={@west}
          names={@names}
          status={@statuses[@west.name]}
        />
        <.player_board
          id="my-board"
          label="You"
          player={@me}
          names={@names}
          status={@statuses[@me.name]}
          mine
          class="order-first md:col-span-2 lg:order-none lg:col-span-1"
        />
        <.player_board
          id="east-panel"
          label="East"
          player={@east}
          names={@names}
          status={@statuses[@east.name]}
        />
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

  @impl true
  def mount(%{"game_id" => game_id}, _session, socket) do
    case Games.fetch_game(game_id) do
      {:ok, game} -> mount_game(socket, game, socket.assigns.current_scope.user)
      {:error, reason} -> {:ok, leave(socket, reason, nil)}
    end
  end

  defp mount_game(socket, game, user) do
    case authorize(game, user) do
      :ok -> mount_authorized(socket, game, user)
      {:error, :unauthorized} -> {:ok, leave(socket, :unauthorized, nil)}
    end
  end

  defp mount_authorized(socket, game, user) do
    if game.status == "finished" do
      mount_finished(socket, game, user)
    else
      mount_active(socket, game, user)
    end
  end

  defp mount_active(socket, game, user) do
    case Games.view(game.id, user.id) do
      {:ok, view} ->
        history = fetch_history(game.id)
        {:ok, socket |> setup(game, user) |> assign_view(view) |> assign(:history, history)}

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
      {:ok, %{view: view, history: history}} ->
        {:ok,
         socket
         |> assign(:boards_available, true)
         |> assign_view(view)
         |> assign(:history, history)}

      {:error, reason} ->
        Logger.warning("Game #{game.id} boards unavailable: #{inspect(reason)}")

        {:ok,
         socket
         |> assign(:boards_available, false)
         |> assign(:history, [])
         |> assign_finished_view(game, user)}
    end
  end

  defp fetch_history(game_id) do
    case Games.history(game_id) do
      {:ok, history} -> history
      {:error, _reason} -> []
    end
  end

  defp setup(socket, game, user) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
      {:ok, _ref} = Presence.track(self(), Games.topic(game.id), user.id, %{name: user.name})
    end

    names = Map.new(Games.players(game), &{to_string(&1.user_id), &1.user.name})

    socket
    |> assign(
      page_title: "7 Wonders",
      game: game,
      names: names,
      info_card: nil,
      discard_minimised: false,
      boards_available: true,
      last_choice: nil
    )
    |> assign_connected()
  end

  # A view shaped like the engine's game-over view, built without booting it:
  # `final_scores` supplies the scores, `players` only the names the top bar
  # needs, and the fixed end state supplies the phase (game over only happens
  # in age III, after the sixth turn discards every last card).
  defp assign_finished_view(socket, game, user) do
    ids = for player <- Games.players(game), do: to_string(player.user_id)
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

  defp refresh(socket) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    case Games.view(game.id, user.id) do
      {:ok, view} ->
        history = fetch_history(game.id)

        socket
        |> animate_new_events(history, view)
        |> assign(:history, history)
        |> assign(:last_choice, GameFormat.action_card(view.my_pending))
        |> assign_view(view)

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

      {:battles, _}, socket ->
        socket
    end)
  end

  defp assign_view(socket, view) do
    by_name = Map.new(view.players, &{&1.name, &1})
    others = GameFormat.others(view)

    socket
    |> assign(
      view: view,
      me: Map.fetch!(by_name, view.me),
      west: Map.fetch!(by_name, view.west),
      east: Map.fetch!(by_name, view.east),
      others: others,
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

  defp assign_connected(socket) do
    assign(socket, :connected, Games.connected_user_ids(Games.topic(socket.assigns.game.id)))
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

defmodule HeliosWeb.GameLive do
  @moduledoc """
  The game table. The page never holds engine state of its own: it re-fetches
  the player's view on every `{:game_updated, seq}` and sends choices as
  `{card, kind, option index}`; the GameServer resolves them against its view.
  """
  use HeliosWeb, :live_view

  import HeliosWeb.GameComponents.Board
  import HeliosWeb.GameComponents.ExtraTurn
  import HeliosWeb.GameComponents.Hand
  import HeliosWeb.GameComponents.PlayerPanels
  import HeliosWeb.GameComponents.Scoreboard
  import HeliosWeb.GameComponents.TopBar

  alias Helios.{Games, Lobbies}
  alias Helios.Games.Choice
  alias HeliosWeb.{GameFormat, Presence}

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope} notifications={@notifications}>
      <div
        id="game"
        class="flex min-h-dvh flex-col bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed"
      >
        <div class="mx-auto flex w-full max-w-7xl flex-1 flex-col gap-3 px-2 pt-3 sm:gap-4 sm:px-4 sm:pt-4 lg:px-8">
          <.top_bar view={@view} names={@names} connected={@connected} />
          <%= if @view.phase.kind == :game_over do %>
            <div class="pb-4">
              <.scoreboard
                scores={@view.scores}
                names={@names}
                me={@view.me}
                lobby_id={@game.lobby_id}
              />
            </div>
          <% else %>
            <div id="table" class="flex flex-1 flex-col gap-3 pb-1 sm:gap-4">
              <.others_strip :if={@others != []} players={@others} names={@names} />
              <div class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-[1fr_2fr_1fr]">
                <.neighbour_panel id="west-panel" label="West" player={@west} names={@names} />
                <.my_board player={@me} />
                <.neighbour_panel id="east-panel" label="East" player={@east} names={@names} />
              </div>
            </div>
            <div
              :if={GameFormat.dock?(@view)}
              id="dock"
              class="sticky bottom-0 z-30 -mx-2 flex flex-col gap-2 rounded-t-xl bg-antique/95 px-2 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] shadow-[0_-4px_12px_rgba(0,0,0,0.2)] sm:mx-0 sm:px-3"
            >
              <.extra_turn_notice view={@view} names={@names} />
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
            <.discard_picker view={@view} />
            <.action_panel :if={@selected_card} card={@selected_card} />
          <% end %>
        </div>
      </div>
    </Layouts.app>
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
      {:ok, view} -> {:ok, socket |> setup(game, user) |> assign_view(view)}
      {:error, reason} -> {:ok, leave(socket, reason, game.lobby_id)}
    end
  end

  # A finished game is read-only: the GameServer is never started for it (which
  # would raise `:not_active`), so the table renders from the scores the finish
  # path persisted on the `games` row.
  defp mount_finished(socket, game, user) do
    {:ok, socket |> setup(game, user) |> assign_finished_view(game, user)}
  end

  defp setup(socket, game, user) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
      {:ok, _ref} = Presence.track(self(), Games.topic(game.id), user.id, %{name: user.name})
    end

    names = Map.new(Games.players(game), &{to_string(&1.user_id), &1.user.name})

    socket
    |> assign(page_title: "7 Wonders", game: game, names: names)
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

  def handle_event("submit", params, socket) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    result =
      with {:ok, choice} <- Choice.parse(params) do
        Games.submit_choice(game.id, user.id, choice)
      end

    socket =
      case result do
        :ok -> assign_selection(socket, nil)
        {:error, reason} -> put_flash(socket, :error, Games.error_message(reason))
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
      {:ok, view} -> assign_view(socket, view)
      {:error, reason} -> leave(socket, reason, game.lobby_id)
    end
  end

  defp assign_view(socket, view) do
    by_name = Map.new(view.players, &{&1.name, &1})
    others = Enum.reject(view.players, &(&1.name in [view.me, view.west, view.east]))

    socket
    |> assign(
      view: view,
      me: Map.fetch!(by_name, view.me),
      west: Map.fetch!(by_name, view.west),
      east: Map.fetch!(by_name, view.east),
      others: others
    )
    |> assign_selection(socket.assigns[:selected])
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

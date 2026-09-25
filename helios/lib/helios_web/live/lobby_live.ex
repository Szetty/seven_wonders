defmodule HeliosWeb.LobbyLive do
  @moduledoc """
  A table ("lobby"). The owner invites online players and removes invitees;
  invited players see who is currently connected.
  """
  use HeliosWeb, :live_view

  alias Helios.Lobbies
  alias HeliosWeb.OnlineTracker
  alias HeliosWeb.Presence
  alias Phoenix.Socket.Broadcast

  @online_topic "users:online"

  @impl true
  def mount(%{"game_id" => game_id}, _session, socket) do
    user = socket.assigns.current_scope.user

    with {:ok, lobby} <- Lobbies.fetch_lobby(game_id),
         :ok <- Lobbies.authorize(lobby, user) do
      {:ok, setup(socket, lobby, user)}
    else
      {:error, reason} ->
        own = Lobbies.get_or_create_own_lobby(user)

        {:ok,
         socket
         |> put_flash(:error, Lobbies.error_message(reason))
         |> push_navigate(to: ~p"/lobby/#{own.id}")}
    end
  end

  defp setup(socket, lobby, user) do
    topic = Lobbies.lobby_topic(lobby.id)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, topic)
      Phoenix.PubSub.subscribe(Helios.PubSub, @online_topic)
      {:ok, _ref} = Presence.track(self(), topic, user.id, %{name: user.name})
    end

    socket
    |> assign(:lobby, lobby)
    |> assign(:owner?, lobby.owner_id == user.id)
    |> assign(:online, OnlineTracker.new(Presence.list(@online_topic)))
    |> assign(:invite_form, invite_form(""))
    |> assign(:connected_ids, MapSet.new([user.id]))
    |> load_members()
    |> load_connected()
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section id="lobby" class="mx-auto w-full max-w-3xl space-y-6 px-4 py-10">
        <h1 id="lobby-title" class="text-2xl font-semibold tracking-tight text-zinc-900">
          {@page_title}
        </h1>

        <.form
          :if={@owner?}
          for={@invite_form}
          id="invite-form"
          phx-change="select_invitee"
          phx-submit="invite"
          class="flex items-end gap-3"
        >
          <div class="flex-1">
            <.input
              field={@invite_form[:user_id]}
              type="select"
              label="Invite a player"
              options={@invite_options}
              prompt="Select username"
            />
          </div>
          <.button
            id="invite-button"
            type="submit"
            disabled={@invite_form[:user_id].value in [nil, ""]}
            class="mb-2 rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow-sm transition hover:bg-zinc-700 disabled:cursor-not-allowed disabled:opacity-40"
          >
            Invite
          </.button>
        </.form>

        <div class="overflow-hidden rounded-xl bg-antique shadow-md ring-1 ring-zinc-900/10">
          <table id="members-table" class="w-full text-left">
            <thead class="bg-zinc-900 text-xs uppercase tracking-wider text-white">
              <tr>
                <th class="px-4 py-3">Username</th>
                <th :if={@owner?} class="w-24 px-4 py-3 text-right">Delete</th>
              </tr>
            </thead>
            <tbody>
              <tr
                :for={row <- @rows}
                id={"member-#{row.user.id}"}
                data-member
                data-name={row.user.name}
                data-leader={to_string(row.leader?)}
                data-self={to_string(row.self?)}
                data-connected={to_string(row.connected?)}
                class={[
                  "border-t border-zinc-900/10 transition-colors",
                  if(row.connected?, do: "text-zinc-900", else: "bg-disconnected text-zinc-500")
                ]}
              >
                <td class="px-4 py-3">
                  <span class="inline-flex items-center gap-2 font-medium">
                    <.icon :if={row.self?} name="hero-chevron-double-right" class="size-4" />
                    <.crown :if={row.leader?} />
                    {row.user.name}
                  </span>
                </td>
                <td :if={@owner?} class="px-4 py-3 text-right">
                  <button
                    :if={!row.self?}
                    id={"uninvite-#{row.user.id}"}
                    type="button"
                    phx-click="uninvite"
                    phx-value-id={row.user.id}
                    aria-label={"Remove #{row.user.name}"}
                    class="inline-flex size-8 items-center justify-center rounded-md bg-zinc-900 text-white transition hover:bg-red-700"
                  >
                    <.icon name="hero-x-mark" class="size-4" />
                  </button>
                </td>
              </tr>
              <tr
                :for={n <- 1..@free_slots//1}
                id={"free-slot-#{n}"}
                data-free-slot
                class="border-t border-zinc-900/10 text-zinc-400"
              >
                <td colspan="2" class="px-4 py-3 text-sm font-semibold uppercase tracking-widest">
                  FREE
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>
    </Layouts.app>
    """
  end

  # Heroicons has no crown; small inline SVG in the same style.
  defp crown(assigns) do
    ~H"""
    <svg
      data-icon="crown"
      role="img"
      aria-label="Leader"
      viewBox="0 0 24 24"
      fill="currentColor"
      class="size-4 text-amber-500"
    >
      <path d="M3 7l4.5 4L12 4l4.5 7L21 7l-2 11H5L3 7zm2 13h14v2H5v-2z" />
    </svg>
    """
  end

  @impl true
  def handle_event("select_invitee", %{"invite" => %{} = params}, socket) do
    {:noreply, assign(socket, :invite_form, invite_form(params["user_id"] || ""))}
  end

  def handle_event("select_invitee", _params, socket), do: {:noreply, socket}

  def handle_event("invite", %{"invite" => %{"user_id" => user_id}}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.invite(scope, lobby, user_id) do
      {:ok, _invite} ->
        {:noreply, socket |> assign(:invite_form, invite_form("")) |> load_members()}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("invite", _params, socket) do
    {:noreply, put_flash(socket, :error, Lobbies.error_message(:invalid_user))}
  end

  def handle_event("uninvite", %{"id" => user_id}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.uninvite(scope, lobby, user_id) do
      :ok -> {:noreply, load_members(socket)}
      {:error, reason} -> {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("uninvite", _params, socket) do
    {:noreply, put_flash(socket, :error, Lobbies.error_message(:not_invited))}
  end

  @impl true
  def handle_info(%Broadcast{event: "presence_diff", topic: @online_topic, payload: diff}, socket) do
    {online, _events} = OnlineTracker.handle_diff(socket.assigns.online, diff)
    {:noreply, socket |> assign(:online, online) |> assign_derived()}
  end

  def handle_info(%Broadcast{event: "presence_diff"}, socket) do
    {:noreply, load_connected(socket)}
  end

  def handle_info({:confirm_offline, user_id, token}, socket) do
    {online, _events} =
      OnlineTracker.confirm_offline(
        socket.assigns.online,
        user_id,
        token,
        Presence.list(@online_topic)
      )

    {:noreply, socket |> assign(:online, online) |> assign_derived()}
  end

  def handle_info({:members_changed}, socket), do: {:noreply, load_members(socket)}

  def handle_info({:declined, _user}, socket), do: {:noreply, load_members(socket)}

  defp invite_form(user_id), do: to_form(%{"user_id" => user_id}, as: :invite)

  defp load_members(socket) do
    members = Lobbies.members(socket.assigns.lobby)
    [%{user: owner} | _] = members
    title = if socket.assigns.owner?, do: "My table", else: "#{owner.name}'s table"

    socket
    |> assign(:members, members)
    |> assign(:page_title, title)
    |> assign_derived()
  end

  defp load_connected(socket) do
    %{lobby: lobby, current_scope: %{user: me}} = socket.assigns

    connected_ids =
      lobby.id
      |> Lobbies.lobby_topic()
      |> Presence.list()
      |> Map.keys()
      |> MapSet.new(&OnlineTracker.to_user_id/1)
      # Whoever renders this page is at the table.
      |> MapSet.put(me.id)

    socket |> assign(:connected_ids, connected_ids) |> assign_derived()
  end

  defp assign_derived(socket) do
    %{
      members: members,
      connected_ids: connected_ids,
      owner?: owner?,
      online: online,
      invite_form: form,
      current_scope: %{user: me}
    } = socket.assigns

    rows =
      members
      |> Enum.map(fn %{user: user, leader?: leader?} ->
        %{
          user: user,
          leader?: leader?,
          connected?: MapSet.member?(connected_ids, user.id),
          self?: user.id == me.id
        }
      end)
      |> Enum.filter(fn row -> owner? or row.connected? end)

    member_ids = MapSet.new(members, & &1.user.id)

    invite_options =
      online
      |> OnlineTracker.online_users()
      |> Enum.reject(fn {id, _name} -> id == me.id or MapSet.member?(member_ids, id) end)
      |> Enum.sort_by(fn {_id, name} -> name end)
      |> Enum.map(fn {id, name} -> {name, id} end)

    selected = form[:user_id].value

    form =
      if Enum.any?(invite_options, fn {_name, id} -> to_string(id) == selected end),
        do: form,
        else: invite_form("")

    socket
    |> assign(:rows, rows)
    |> assign(
      :free_slots,
      if(owner?, do: max(Lobbies.max_players() - length(members), 0), else: 0)
    )
    |> assign(:invite_options, invite_options)
    |> assign(:invite_form, form)
  end
end

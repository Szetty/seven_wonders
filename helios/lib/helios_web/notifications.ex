defmodule HeliosWeb.Notifications do
  @moduledoc """
  Per-user notification strip for every authenticated LiveView.

  Two kinds:

    * `:approve` — a pending table invite ("You are expected on table X"),
      persistent until accepted, declined or withdrawn.
    * `:simple` — transient messages with an OK button, auto-expiring after 8 s.

  At most 5 are shown, newest first; approve notifications are never evicted
  to make room for simple ones.
  """
  import Phoenix.Component, only: [assign: 3, update: 3]
  import Phoenix.LiveView

  use HeliosWeb, :verified_routes

  alias Helios.Lobbies

  @max_visible 5
  @simple_ttl_ms 8_000
  @events ~w(accept_invite decline_invite dismiss_notification)

  @type notification :: %{
          id: String.t(),
          kind: :approve | :simple,
          message: String.t(),
          lobby_id: String.t() | nil
        }

  def on_mount(:default, _params, _session, socket) do
    user = socket.assigns.current_scope.user

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(user.id))
    end

    notifications = user |> Lobbies.pending_invites() |> Enum.map(&approve/1)

    {:cont,
     socket
     |> assign(:notifications, notifications)
     |> attach_hook(:notifications, :handle_info, &handle_info/2)
     |> attach_hook(:notifications, :handle_event, &handle_event/3)}
  end

  @doc "Adds a transient notification to the socket and schedules its expiry."
  def push_simple(socket, message) do
    notification = simple(message)
    Process.send_after(self(), {:expire_notification, notification.id}, @simple_ttl_ms)
    update(socket, :notifications, &add(&1, notification))
  end

  @spec approve(%{lobby_id: String.t(), owner_name: String.t()}) :: notification()
  def approve(%{lobby_id: lobby_id, owner_name: owner_name}) do
    %{
      id: approve_id(lobby_id),
      kind: :approve,
      message: "You are expected on table #{owner_name}",
      lobby_id: lobby_id
    }
  end

  @spec simple(String.t()) :: notification()
  def simple(message) do
    %{
      id: "simple-#{System.unique_integer([:positive, :monotonic])}",
      kind: :simple,
      message: message,
      lobby_id: nil
    }
  end

  @doc "Prepends (de-duplicating by id), then evicts the oldest simple ones beyond 5."
  @spec add([notification()], notification()) :: [notification()]
  def add(notifications, %{id: id} = notification) do
    evict_simple([notification | Enum.reject(notifications, &(&1.id == id))])
  end

  @spec remove([notification()], String.t()) :: [notification()]
  def remove(notifications, id), do: Enum.reject(notifications, &(&1.id == id))

  @spec visible([notification()]) :: [notification()]
  def visible(notifications), do: Enum.take(notifications, @max_visible)

  defp evict_simple(list) when length(list) <= @max_visible, do: list

  defp evict_simple(list) do
    case list |> Enum.reverse() |> Enum.find_index(&(&1.kind == :simple)) do
      nil -> list
      reverse_index -> list |> List.delete_at(length(list) - 1 - reverse_index) |> evict_simple()
    end
  end

  defp approve_id(lobby_id), do: "invite-#{lobby_id}"

  defp handle_info({:invited, invite}, socket) do
    notification = approve(%{lobby_id: invite.lobby_id, owner_name: invite.lobby.owner.name})
    {:halt, update(socket, :notifications, &add(&1, notification))}
  end

  defp handle_info({:uninvited, lobby_id}, socket) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))
    message = removed_message(Lobbies.owner_name(lobby_id))

    case socket.assigns do
      %{lobby: %{id: ^lobby_id}} ->
        own = Lobbies.get_or_create_own_lobby(socket.assigns.current_scope.user)
        {:halt, socket |> put_flash(:info, message) |> push_navigate(to: ~p"/lobby/#{own.id}")}

      _ ->
        {:halt, push_simple(socket, message)}
    end
  end

  defp handle_info({:invite_resolved, lobby_id}, socket) do
    {:halt, update(socket, :notifications, &remove(&1, approve_id(lobby_id)))}
  end

  defp handle_info({:expire_notification, id}, socket) do
    {:halt, update(socket, :notifications, &remove(&1, id))}
  end

  defp handle_info(_message, socket), do: {:cont, socket}

  defp handle_event("accept_invite", %{"id" => lobby_id}, socket) when is_binary(lobby_id) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))

    case Lobbies.accept(socket.assigns.current_scope, lobby_id) do
      :ok -> {:halt, push_navigate(socket, to: ~p"/lobby/#{lobby_id}")}
      {:error, reason} -> {:halt, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  defp handle_event("decline_invite", %{"id" => lobby_id}, socket) when is_binary(lobby_id) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))

    case Lobbies.decline(socket.assigns.current_scope, lobby_id) do
      :ok -> {:halt, socket}
      {:error, reason} -> {:halt, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  defp handle_event("dismiss_notification", %{"id" => id}, socket) when is_binary(id) do
    {:halt, update(socket, :notifications, &remove(&1, id))}
  end

  # Malformed params for our own events: swallow, never crash the LiveView.
  defp handle_event(event, _params, socket) when event in @events, do: {:halt, socket}

  defp handle_event(_event, _params, socket), do: {:cont, socket}

  defp removed_message(nil), do: "You were removed from a table"
  defp removed_message(owner_name), do: "You were removed from #{owner_name}'s table"
end

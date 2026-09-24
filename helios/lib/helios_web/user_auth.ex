defmodule HeliosWeb.UserAuth do
  @moduledoc """
  Session authentication.

    * `fetch_current_scope/2`: router plug assigning `:current_scope`.
    * `log_in_user/3` / `log_out_user/1`: used by `HeliosWeb.UserSessionController`.
    * `on_mount/4` hooks for `live_session`s:
      * `:require_user`: authenticated pages; tracks the user in
        `HeliosWeb.Presence` (`"users:online"`) once connected.
      * `:redirect_if_user`: guest-only pages.
  """
  use HeliosWeb, :verified_routes

  import Plug.Conn
  import Phoenix.Controller

  alias Helios.Accounts
  alias Helios.Accounts.{Scope, User}

  @doc """
  Assigns `:current_scope` from the session's `:user_token`. An unknown,
  revoked or expired token is removed from the session. Only the auth keys
  are removed, so flash messages survive.
  """
  def fetch_current_scope(conn, _opts) do
    case get_session(conn, :user_token) do
      nil -> assign(conn, :current_scope, nil)
      token -> assign_scope_for_token(conn, token)
    end
  end

  defp assign_scope_for_token(conn, token) do
    case Accounts.get_user_by_session_token(token) do
      %User{} = user ->
        assign(conn, :current_scope, Scope.for_user(user))

      nil ->
        conn
        |> delete_session(:user_token)
        |> delete_session(:live_socket_id)
        |> assign(:current_scope, nil)
    end
  end

  @doc """
  Starts a fresh session for `token` (from `Accounts.login/2`) and redirects to `/`.
  """
  def log_in_user(conn, %User{} = _user, token) when is_binary(token) do
    conn
    |> renew_session()
    |> put_session(:user_token, token)
    |> put_session(:live_socket_id, Accounts.live_socket_id(token))
    |> redirect(to: ~p"/")
  end

  @doc """
  Deletes the session token, disconnects the session's LiveViews, clears the
  session and redirects to `/login`.
  """
  def log_out_user(conn) do
    if token = get_session(conn, :user_token) do
      Accounts.delete_session_token(token)
    end

    if live_socket_id = get_session(conn, :live_socket_id) do
      HeliosWeb.Endpoint.broadcast(live_socket_id, "disconnect", %{})
    end

    conn
    |> renew_session()
    |> redirect(to: ~p"/login")
  end

  # Renews the session id and erases all session data (prevents fixation).
  defp renew_session(conn) do
    delete_csrf_token()

    conn
    |> configure_session(renew: true)
    |> clear_session()
  end

  @doc """
  LiveView hooks, used via `live_session ..., on_mount: [{HeliosWeb.UserAuth, hook}]`.
  """
  def on_mount(:require_user, _params, session, socket) do
    socket = mount_current_scope(socket, session)

    case socket.assigns.current_scope do
      %Scope{user: user} ->
        if Phoenix.LiveView.connected?(socket) do
          HeliosWeb.Presence.track_user(self(), user)
        end

        {:cont, socket}

      nil ->
        reason = if session["user_token"], do: :invalid_user, else: :unauthorized

        socket =
          socket
          |> Phoenix.LiveView.put_flash(:error, message(reason))
          |> Phoenix.LiveView.redirect(to: ~p"/login")

        {:halt, socket}
    end
  end

  def on_mount(:redirect_if_user, _params, session, socket) do
    socket = mount_current_scope(socket, session)

    if socket.assigns.current_scope do
      {:halt, Phoenix.LiveView.redirect(socket, to: ~p"/")}
    else
      {:cont, socket}
    end
  end

  # On the dead render this reuses the plug's assign; on connect it reads the session.
  defp mount_current_scope(socket, session) do
    Phoenix.Component.assign_new(socket, :current_scope, fn ->
      with token when is_binary(token) <- session["user_token"],
           %User{} = user <- Accounts.get_user_by_session_token(token) do
        Scope.for_user(user)
      else
        _ -> nil
      end
    end)
  end

  defp message(:unauthorized), do: "You must log in to access this page."
  defp message(:invalid_user), do: "Your session has expired, please log in again."
end

defmodule HeliosWeb.PageController do
  use HeliosWeb, :controller

  alias Helios.Accounts.Scope

  @doc "`/`: logged-in users go to their lobby, guests to the login page."
  def home(conn, _params) do
    case conn.assigns.current_scope do
      %Scope{} -> redirect(conn, to: ~p"/lobby")
      nil -> redirect(conn, to: ~p"/login")
    end
  end
end

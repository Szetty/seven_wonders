defmodule BackendWeb.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use BackendWeb, :controller

  def call(conn, {:error, status_code, type, msg}) do
    conn
    |> assign(:error_message, msg)
    |> assign(:error_type, type)
    |> put_status(status_code)
    |> put_view(BackendWeb.ErrorView)
    |> render("error.json")
  end
end

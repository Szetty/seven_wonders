defmodule BackendWeb.PingController do
  use BackendWeb, :controller

  def ping(conn, _) do
    text(conn, "OK")
  end
end

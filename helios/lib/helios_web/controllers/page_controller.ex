defmodule HeliosWeb.PageController do
  use HeliosWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end

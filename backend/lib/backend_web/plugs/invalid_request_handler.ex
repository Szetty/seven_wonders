defmodule BackendWeb.Plugs.InvalidRequestHandler do
  use BackendWeb, :controller
  alias BackendWeb.Common.ErrorTypes

  @impl Plug
  def init(errors), do: errors

  @impl Plug
  def call(conn, errors) when is_list(errors) do
    conn
    |> assign(:error_message, "Invalid request body")
    |> assign(:error_type, ErrorTypes.invalid_body())
    |> assign(:errors, Enum.map(errors, &render_error/1))
    |> put_status(400)
    |> put_view(BackendWeb.ErrorView)
    |> render("error.json")
  end

  defp render_error(error) do
    pointer = OpenApiSpex.path_to_string(error)

    %{
      title: "Invalid value",
      source: %{
        pointer: pointer
      },
      detail: to_string(error)
    }
  end
end

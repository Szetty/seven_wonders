defmodule BackendWeb.Schemas.GameID do
  require OpenApiSpex

  OpenApiSpex.schema(%{
    type: :string,
    description: "Game ID associated to a user",
    format: "uuid"
  })
end

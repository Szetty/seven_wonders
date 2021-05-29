defmodule BackendWeb.Schemas.JWT do
  require OpenApiSpex

  @jwt_regex ~r(^[A-Za-z0-9-_=]+\.[A-Za-z0-9-_=]+\.?[A-Za-z0-9-_.+/=]*$)

  OpenApiSpex.schema(%{
    type: :string,
    pattern: @jwt_regex,
    description: "JWT token"
  })
end

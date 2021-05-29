defmodule BackendWeb.Schemas.ErrorResponse do
  require OpenApiSpex
  alias OpenApiSpex.{Operation, Schema}

  OpenApiSpex.schema(%{
    type: :object,
    properties: %{
      error_type: %Schema{type: :string},
      error_message: %Schema{type: :string},
      errors: %Schema{
        type: :array,
        items: %Schema{type: :object}
      }
    },
    required: [:error_type, :error_message, :errors]
  })

  def response do
    Operation.response(
      "ErrorResponse",
      "application/json",
      __MODULE__
    )
  end
end

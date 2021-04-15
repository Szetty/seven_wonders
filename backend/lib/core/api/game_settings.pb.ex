defmodule Core.Api.GameSettings.Reply do
  @moduledoc false
  use Protobuf, syntax: :proto2

  @type t :: %__MODULE__{
          version: String.t(),
          supported_wonders: [String.t()]
        }

  defstruct [:version, :supported_wonders]

  field(:version, 1, optional: true, type: :string)
  field(:supported_wonders, 2, repeated: true, type: :string)
end

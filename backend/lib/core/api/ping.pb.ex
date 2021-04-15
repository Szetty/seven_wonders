defmodule Core.Api.Ping.PingRequest do
  @moduledoc false
  use Protobuf, syntax: :proto2

  @type t :: %__MODULE__{
          name: String.t()
        }

  defstruct [:name]

  field(:name, 1, optional: true, type: :string)
end

defmodule Core.Api.Ping.PingReply do
  @moduledoc false
  use Protobuf, syntax: :proto2

  @type t :: %__MODULE__{
          message: String.t()
        }

  defstruct [:message]

  field(:message, 1, optional: true, type: :string)
end

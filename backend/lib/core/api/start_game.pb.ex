defmodule Core.Api.StartGame.Request.WonderSide do
  @moduledoc false
  use Protobuf, syntax: :proto2

  @type t :: %__MODULE__{
          wonder_name: String.t(),
          is_side_b: boolean
        }

  defstruct [:wonder_name, :is_side_b]

  field(:wonder_name, 1, optional: true, type: :string)
  field(:is_side_b, 2, optional: true, type: :bool)
end

defmodule Core.Api.StartGame.Request do
  @moduledoc false
  use Protobuf, syntax: :proto2

  @type t :: %__MODULE__{
          players: [String.t()],
          wonder_sides: [Core.Api.StartGame.Request.WonderSide.t()]
        }

  defstruct [:players, :wonder_sides]

  field(:players, 1, repeated: true, type: :string)
  field(:wonder_sides, 2, repeated: true, type: Core.Api.StartGame.Request.WonderSide)
end

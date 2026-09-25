defmodule Helios.Accounts.Scope do
  @moduledoc """
  The caller's scope (Phoenix 1.8 convention), assigned as `@current_scope`.

  Guests have `current_scope == nil`; logged-in callers get
  `%Scope{user: %User{}}`. Contexts receive the scope to know who is acting.
  """
  alias Helios.Accounts.User

  defstruct user: nil

  @type t :: %__MODULE__{user: User.t() | nil}

  @doc "Builds the scope for a user; `nil` for guests."
  @spec for_user(User.t() | nil) :: t() | nil
  def for_user(%User{} = user), do: %__MODULE__{user: user}
  def for_user(nil), do: nil
end

defmodule Helios.Accounts.ScopeTest do
  use ExUnit.Case, async: true

  alias Helios.Accounts.{Scope, User}

  test "for_user/1 wraps a user" do
    user = %User{id: 1, name: "alice"}
    assert Scope.for_user(user) == %Scope{user: user}
  end

  test "for_user/1 returns nil for guests" do
    assert Scope.for_user(nil) == nil
  end
end

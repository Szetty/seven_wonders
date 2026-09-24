defmodule Helios.Accounts.UserTokenTest do
  use ExUnit.Case, async: true

  alias Helios.Accounts.{User, UserToken}

  test "build_session_token/1 returns a 32-byte raw token and stores only its SHA-256 hash" do
    {token, user_token} = UserToken.build_session_token(%User{id: 42})

    assert byte_size(token) == 32
    assert user_token.user_id == 42
    assert user_token.token_hash == :crypto.hash(:sha256, token)
    refute user_token.token_hash == token
  end

  test "build_session_token/1 is random" do
    {a, _} = UserToken.build_session_token(%User{id: 1})
    {b, _} = UserToken.build_session_token(%User{id: 1})
    refute a == b
  end

  test "live_socket_id/1 derives the disconnect topic from the hash" do
    hash = UserToken.hash_token("raw-token")
    assert hash == :crypto.hash(:sha256, "raw-token")
    assert UserToken.live_socket_id(hash) == "users_sessions:" <> Base.url_encode64(hash)
  end
end

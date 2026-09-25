defmodule Helios.LobbiesFixtures do
  @moduledoc """
  Test helpers for lobby tests. Players are created through
  `Helios.Accounts.login/2`, exactly like a real login.
  """

  alias Helios.Accounts
  alias Helios.Lobbies

  def unique_player_name, do: "p#{System.unique_integer([:positive])}"

  def player_with_token_fixture(name \\ unique_player_name()) do
    {:ok, user, token} = Accounts.login(Application.fetch_env!(:helios, :access_token), name)
    {user, token}
  end

  def player_fixture(name \\ unique_player_name()) do
    {user, _token} = player_with_token_fixture(name)
    user
  end

  def lobby_fixture(owner), do: Lobbies.get_or_create_own_lobby(owner)

  def invite_fixture(lobby, owner, invitee) do
    {:ok, invite} = Lobbies.invite(Helios.Accounts.Scope.for_user(owner), lobby, invitee.id)
    invite
  end
end

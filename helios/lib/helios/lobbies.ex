defmodule Helios.Lobbies do
  @moduledoc """
  Tables ("lobbies").

  Every user owns exactly one lobby, created lazily; its owner is its leader
  forever. The owner invites other users; an invite row (any status) makes the
  invitee *authorized* to open the lobby. Leaving a lobby never removes
  authorization.

  PubSub messages (server `Helios.PubSub`):

    * `"user:<user_id>"` — `{:invited, %Invite{lobby: %Lobby{owner: %User{}}}}`,
      `{:uninvited, lobby_id}`, `{:invite_resolved, lobby_id}`
    * `"lobby:<lobby_id>"` — `{:members_changed}`, `{:declined, %User{}}`
  """

  import Ecto.Query, warn: false

  alias Helios.Accounts.User
  alias Helios.Lobbies.Invite
  alias Helios.Lobbies.Lobby
  alias Helios.Repo

  @max_players 7

  @type member :: %{user: User.t(), leader?: boolean()}

  @doc "Maximum number of players at a table, owner included."
  @spec max_players() :: pos_integer()
  def max_players, do: @max_players

  @spec lobby_topic(Ecto.UUID.t()) :: String.t()
  def lobby_topic(lobby_id), do: "lobby:#{lobby_id}"

  @spec user_topic(integer()) :: String.t()
  def user_topic(user_id), do: "user:#{user_id}"

  @doc """
  Returns the user's own lobby, creating it on first use. Race-safe: a
  concurrent insert is ignored through the unique owner index and re-fetched.
  """
  @spec get_or_create_own_lobby(User.t()) :: Lobby.t()
  def get_or_create_own_lobby(%User{id: user_id}) do
    case Repo.get_by(Lobby, owner_id: user_id) do
      %Lobby{} = lobby ->
        lobby

      nil ->
        Repo.insert!(%Lobby{owner_id: user_id},
          on_conflict: :nothing,
          conflict_target: [:owner_id]
        )

        Repo.get_by!(Lobby, owner_id: user_id)
    end
  end

  @doc "Looks a lobby up by its public game id."
  @spec fetch_lobby(term()) :: {:ok, Lobby.t()} | {:error, :invalid_game_id}
  def fetch_lobby(game_id) do
    with {:ok, id} <- cast_lobby_id(game_id),
         %Lobby{} = lobby <- Repo.get(Lobby, id) do
      {:ok, lobby}
    else
      _ -> {:error, :invalid_game_id}
    end
  end

  @doc "The owner and every invited user (any status) may open the lobby."
  @spec authorize(Lobby.t(), User.t()) :: :ok | {:error, :unauthorized}
  def authorize(%Lobby{owner_id: owner_id}, %User{id: owner_id}), do: :ok

  def authorize(%Lobby{id: lobby_id}, %User{id: user_id}) do
    if Repo.exists?(from i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user_id) do
      :ok
    else
      {:error, :unauthorized}
    end
  end

  @doc "Owner first (leader), then invitees in invitation order."
  @spec members(Lobby.t()) :: [member()]
  def members(%Lobby{} = lobby) do
    owner = Repo.get!(User, lobby.owner_id)

    invitees =
      Repo.all(
        from i in Invite,
          join: u in assoc(i, :user),
          where: i.lobby_id == ^lobby.id,
          order_by: [asc: i.inserted_at, asc: i.id],
          select: u
      )

    [%{user: owner, leader?: true} | Enum.map(invitees, &%{user: &1, leader?: false})]
  end

  @doc "Human message for every lobby error atom."
  @spec error_message(atom()) :: String.t()
  def error_message(:not_leader), do: "Only the table leader can do that"
  def error_message(:unauthorized), do: "Only invited players can join this table"
  def error_message(:invalid_game_id), do: "That table does not exist"
  def error_message(:self_invite), do: "You can't invite yourself"
  def error_message(:already_invited), do: "That player is already invited"
  def error_message(:lobby_full), do: "The table is full (#{@max_players} players max)"
  def error_message(:invalid_user), do: "That player does not exist"
  def error_message(:not_invited), do: "That invitation is no longer valid"

  defp cast_lobby_id(id) when is_binary(id), do: Ecto.UUID.cast(id)
  defp cast_lobby_id(_id), do: :error
end

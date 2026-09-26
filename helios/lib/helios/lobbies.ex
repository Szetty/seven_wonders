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

  alias Helios.Accounts.Scope
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

  @doc """
  The owner invites `invitee_id` (integer or numeric string).
  """
  @spec invite(Scope.t(), Lobby.t(), term()) ::
          {:ok, Invite.t()}
          | {:error, :not_leader | :self_invite | :already_invited | :lobby_full | :invalid_user}
  def invite(%Scope{user: %User{} = user}, %Lobby{} = lobby, invitee_id) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, invitee_id} <- parse_user_id(invitee_id, :invalid_user),
         :ok <- ensure_not_self(lobby, invitee_id),
         {:ok, invitee} <- fetch_user(invitee_id),
         :ok <- ensure_not_invited(lobby, invitee.id),
         {:ok, invite} <- insert_invite(lobby, invitee) do
      invite = %{invite | lobby: %{lobby | owner: user}, user: invitee}
      broadcast(user_topic(invitee.id), {:invited, invite})
      broadcast(lobby_topic(lobby.id), {:members_changed})
      {:ok, invite}
    end
  end

  @doc "The owner removes an invitee (pending or accepted)."
  @spec uninvite(Scope.t(), Lobby.t(), term()) :: :ok | {:error, :not_leader | :not_invited}
  def uninvite(%Scope{user: %User{} = user}, %Lobby{} = lobby, user_id) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, user_id} <- parse_user_id(user_id, :not_invited),
         {1, _} <-
           Repo.delete_all(
             from i in Invite, where: i.lobby_id == ^lobby.id and i.user_id == ^user_id
           ) do
      broadcast(user_topic(user_id), {:uninvited, lobby.id})
      broadcast(lobby_topic(lobby.id), {:members_changed})
      :ok
    else
      {0, _} -> {:error, :not_invited}
      {:error, _reason} = error -> error
    end
  end

  @doc "The invitee accepts; idempotent for an already accepted invite."
  @spec accept(Scope.t(), term()) :: :ok | {:error, :not_invited}
  def accept(%Scope{user: %User{} = user}, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id),
         {1, _} <-
           Repo.update_all(
             from(i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user.id),
             set: [status: "accepted", updated_at: now()]
           ) do
      broadcast(user_topic(user.id), {:invite_resolved, lobby_id})
      :ok
    else
      _ -> {:error, :not_invited}
    end
  end

  @doc "The invitee declines a pending invite and loses authorization."
  @spec decline(Scope.t(), term()) :: :ok | {:error, :not_invited}
  def decline(%Scope{user: %User{} = user}, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id),
         {1, _} <-
           Repo.delete_all(
             from i in Invite,
               where: i.lobby_id == ^lobby_id and i.user_id == ^user.id and i.status == "pending"
           ) do
      broadcast(lobby_topic(lobby_id), {:declined, user})
      broadcast(user_topic(user.id), {:invite_resolved, lobby_id})
      :ok
    else
      _ -> {:error, :not_invited}
    end
  end

  @doc "Pending invites for `user`, newest first."
  @spec pending_invites(User.t()) :: [%{lobby_id: Ecto.UUID.t(), owner_name: String.t()}]
  def pending_invites(%User{id: user_id}) do
    Repo.all(
      from i in Invite,
        join: l in assoc(i, :lobby),
        join: o in assoc(l, :owner),
        where: i.user_id == ^user_id and i.status == "pending",
        order_by: [desc: i.inserted_at, desc: i.id],
        select: %{lobby_id: l.id, owner_name: o.name}
    )
  end

  @doc "Name of the lobby's owner, or nil for an unknown/malformed id."
  @spec owner_name(term()) :: String.t() | nil
  def owner_name(lobby_id) do
    case cast_lobby_id(lobby_id) do
      {:ok, id} ->
        Repo.one(from l in Lobby, join: o in assoc(l, :owner), where: l.id == ^id, select: o.name)

      :error ->
        nil
    end
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

  defp ensure_leader(%Lobby{owner_id: owner_id}, %User{id: owner_id}), do: :ok
  defp ensure_leader(_lobby, _user), do: {:error, :not_leader}

  defp parse_user_id(id, _error) when is_integer(id) and id > 0, do: {:ok, id}

  defp parse_user_id(id, error) when is_binary(id) do
    case Integer.parse(id) do
      {int, ""} when int > 0 -> {:ok, int}
      _ -> {:error, error}
    end
  end

  defp parse_user_id(_id, error), do: {:error, error}

  defp ensure_not_self(%Lobby{owner_id: owner_id}, owner_id), do: {:error, :self_invite}
  defp ensure_not_self(_lobby, _user_id), do: :ok

  defp fetch_user(user_id) do
    case Repo.get(User, user_id) do
      %User{} = user -> {:ok, user}
      nil -> {:error, :invalid_user}
    end
  end

  defp ensure_not_invited(%Lobby{id: lobby_id}, user_id) do
    if Repo.exists?(from i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user_id) do
      {:error, :already_invited}
    else
      :ok
    end
  end

  defp ensure_capacity(%Lobby{id: lobby_id}) do
    invites = Repo.aggregate(from(i in Invite, where: i.lobby_id == ^lobby_id), :count)

    if invites + 1 >= @max_players, do: {:error, :lobby_full}, else: :ok
  end

  defp insert_invite(%Lobby{id: lobby_id} = lobby, %User{id: user_id}) do
    Repo.transaction(fn ->
      with :ok <- ensure_capacity(lobby),
           {:ok, invite} <-
             %Invite{lobby_id: lobby_id, user_id: user_id}
             |> Invite.create_changeset()
             |> Repo.insert() do
        invite
      else
        {:error, %Ecto.Changeset{}} -> Repo.rollback(:already_invited)
        {:error, reason} -> Repo.rollback(reason)
      end
    end)
  end

  defp broadcast(topic, message), do: Phoenix.PubSub.broadcast(Helios.PubSub, topic, message)

  defp now, do: DateTime.utc_now() |> DateTime.truncate(:second)
end

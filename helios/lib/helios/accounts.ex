defmodule Helios.Accounts do
  @moduledoc """
  Users and sessions.

  There are no passwords: anyone who knows the shared access token can log in
  with a name. A name is *held* while its user is online (tracked in
  `HeliosWeb.Presence` on `"users:online"`). Logging in with a known, un-held
  name re-enters that account and revokes its previous sessions.
  """
  import Ecto.Query, warn: false

  alias Helios.Accounts.{User, UserToken}
  alias Helios.Repo

  @session_validity_in_days 60

  @type login_error :: {:error, :invalid_access_token | :invalid_name, String.t()}

  @doc """
  Validates login input without side effects. The checks run in this order:

    1. access token: empty → "Access token can't be empty!", mismatch → "Wrong access token"
    2. name: see `User.name_changeset/2`
    3. name held by an online user → "Name is already taken"

  Non-string values are treated as empty strings.
  """
  @spec check_login(term(), term()) :: :ok | login_error()
  def check_login(access_token, name) do
    case validate_login(access_token, name) do
      {:ok, _name} -> :ok
      {:error, _reason, _message} = error -> error
    end
  end

  @doc """
  Logs in: re-runs the login validation (the same checks as `check_login/2`), then in one
  transaction gets or inserts the user by (trimmed) name, deletes that user's existing
  session tokens and inserts a new one. After commit, every revoked session's LiveViews
  are told to disconnect.

  Returns the raw session token (32 random bytes) to store in the session.
  """
  @spec login(term(), term()) :: {:ok, User.t(), binary()} | login_error()
  def login(access_token, name) do
    with {:ok, name} <- validate_login(access_token, name) do
      {:ok, {user, token, revoked_hashes}} = Repo.transact(fn -> {:ok, start_session(name)} end)
      disconnect_sessions(revoked_hashes)
      {:ok, user, token}
    end
  end

  @doc """
  Returns the user owning the raw session token, or `nil` when the token is
  unknown, revoked or older than #{@session_validity_in_days} days.
  """
  @spec get_user_by_session_token(term()) :: User.t() | nil
  def get_user_by_session_token(token) when is_binary(token) do
    hash = UserToken.hash_token(token)

    cutoff =
      DateTime.utc_now()
      |> DateTime.add(-@session_validity_in_days, :day)
      |> DateTime.truncate(:second)

    Repo.one(
      from t in UserToken,
        join: u in assoc(t, :user),
        where: t.token_hash == ^hash and t.inserted_at > ^cutoff,
        select: u
    )
  end

  def get_user_by_session_token(_token), do: nil

  @doc "Deletes the session token (no-op when it does not exist)."
  @spec delete_session_token(binary()) :: :ok
  def delete_session_token(token) when is_binary(token) do
    Repo.delete_all(from t in UserToken, where: t.token_hash == ^UserToken.hash_token(token))
    :ok
  end

  @doc """
  The `live_socket_id` for a raw session token:
  `"users_sessions:" <> Base.url_encode64(token_hash)`.
  """
  @spec live_socket_id(binary()) :: String.t()
  def live_socket_id(token) when is_binary(token) do
    token |> UserToken.hash_token() |> UserToken.live_socket_id()
  end

  ## Login internals

  # Returns the normalised (trimmed) name on success.
  defp validate_login(access_token, name) do
    with :ok <- verify_access_token(param_string(access_token)),
         {:ok, name} <- normalize_name(param_string(name)),
         :ok <- ensure_name_available(name) do
      {:ok, name}
    end
  end

  defp param_string(value) when is_binary(value), do: value
  defp param_string(_value), do: ""

  defp verify_access_token(""),
    do: {:error, :invalid_access_token, "Access token can't be empty!"}

  defp verify_access_token(token) do
    if Plug.Crypto.secure_compare(token, Application.fetch_env!(:helios, :access_token)) do
      :ok
    else
      {:error, :invalid_access_token, "Wrong access token"}
    end
  end

  defp normalize_name(name) do
    changeset = User.name_changeset(%User{}, %{name: name})

    if changeset.valid? do
      {:ok, Ecto.Changeset.get_field(changeset, :name)}
    else
      {message, _opts} = Keyword.fetch!(changeset.errors, :name)
      {:error, :invalid_name, message}
    end
  end

  defp ensure_name_available(name) do
    case Repo.get_by(User, name: name) do
      %User{id: id} ->
        if HeliosWeb.Presence.user_online?(id),
          do: {:error, :invalid_name, "Name is already taken"},
          else: :ok

      nil ->
        :ok
    end
  end

  defp start_session(name) do
    user = get_or_insert_user!(name)
    user_tokens = from(t in UserToken, where: t.user_id == ^user.id)
    revoked_hashes = Repo.all(from t in user_tokens, select: t.token_hash)
    {_count, _} = Repo.delete_all(user_tokens)

    {token, user_token} = UserToken.build_session_token(user)
    Repo.insert!(user_token)

    {user, token, revoked_hashes}
  end

  # Race-safe: a concurrent insert of the same name is ignored and re-fetched.
  defp get_or_insert_user!(name) do
    case Repo.get_by(User, name: name) do
      %User{} = user ->
        user

      nil ->
        %User{}
        |> User.name_changeset(%{name: name})
        |> Repo.insert!(on_conflict: :nothing, conflict_target: :name)

        Repo.get_by!(User, name: name)
    end
  end

  defp disconnect_sessions(token_hashes) do
    Enum.each(token_hashes, fn hash ->
      HeliosWeb.Endpoint.broadcast(UserToken.live_socket_id(hash), "disconnect", %{})
    end)
  end
end

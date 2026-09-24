defmodule Helios.Accounts do
  @moduledoc """
  Users and sessions.

  There are no passwords: anyone who knows the shared access token can log in
  with a name. A name is *held* while its user is online (tracked in
  `HeliosWeb.Presence` on `"users:online"`). Logging in with a known, un-held
  name re-enters that account.
  """
  import Ecto.Query, warn: false

  alias Helios.Accounts.User
  alias Helios.Repo

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
end

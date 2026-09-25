defmodule Helios.Accounts.UserToken do
  @moduledoc """
  A login session. Only the SHA-256 hash of the random token is stored; the raw
  token lives in the signed Phoenix session cookie.
  """
  use Ecto.Schema

  alias Helios.Accounts.User

  @hash_algorithm :sha256
  @rand_size 32

  @type t :: %__MODULE__{}

  schema "user_tokens" do
    field :token_hash, :binary
    belongs_to :user, User

    timestamps(type: :utc_datetime, updated_at: false)
  end

  @doc """
  Generates a raw session token and the (unsaved) `UserToken` holding its hash.
  """
  @spec build_session_token(User.t()) :: {binary(), t()}
  def build_session_token(%User{id: user_id}) do
    token = :crypto.strong_rand_bytes(@rand_size)
    {token, %__MODULE__{token_hash: hash_token(token), user_id: user_id}}
  end

  @doc "Hashes a raw session token."
  @spec hash_token(binary()) :: binary()
  def hash_token(token) when is_binary(token), do: :crypto.hash(@hash_algorithm, token)

  @doc """
  The PubSub topic used as the session's `live_socket_id`: broadcasting
  `"disconnect"` on it drops every LiveView opened with that session.
  """
  @spec live_socket_id(binary()) :: String.t()
  def live_socket_id(token_hash) when is_binary(token_hash) do
    "users_sessions:" <> Base.url_encode64(token_hash)
  end
end

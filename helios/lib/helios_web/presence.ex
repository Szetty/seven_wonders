defmodule HeliosWeb.Presence do
  @moduledoc """
  Presence tracking.

  `"users:online"` lists every user with at least one connected authenticated
  LiveView (tracked by `HeliosWeb.UserAuth`'s `:require_user` hook). A name is
  "held" while its user is present here. Keys are user ids **as strings**,
  because `Phoenix.Presence` stringifies keys in `list/1` and in diffs anyway.
  """
  use Phoenix.Presence, otp_app: :helios, pubsub_server: Helios.PubSub

  alias Helios.Accounts.User

  @online_topic "users:online"

  @doc "The global online-users topic."
  @spec online_topic() :: String.t()
  def online_topic, do: @online_topic

  @doc "Tracks `pid` as the given user on the online topic."
  @spec track_user(pid(), User.t()) :: {:ok, binary()} | {:error, term()}
  def track_user(pid, %User{id: id, name: name}) do
    track(pid, @online_topic, to_string(id), %{name: name})
  end

  @doc "Whether the user has at least one tracked process on the online topic."
  @spec user_online?(integer() | String.t()) :: boolean()
  def user_online?(user_id), do: get_by_key(@online_topic, to_string(user_id)) != []
end

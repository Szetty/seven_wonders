defmodule HeliosWeb.OnlineTracker do
  @moduledoc """
  Debounced global online/offline tracking over the `"users:online"` presence
  topic, kept in a LiveView's assigns.

  * A join for a user not previously online emits `{:came_online, id, name}`.
  * A leave schedules `{:confirm_offline, id, token}` to `self()` after the
    grace period (`config :helios, :presence_grace_ms`, default 5 s).
  * A join while a check is pending cancels it and emits nothing (reconnects
    and reloads do not flap).
  * On confirm, a user still absent from the presence list emits
    `{:went_offline, id, name}`; a stale token is ignored.

  Presence keys are stringified by Phoenix, so ids are normalised with
  `to_user_id/1`.
  """

  @topic "users:online"
  @default_grace_ms 5_000

  defstruct online: %{}, pending: %{}, grace_ms: @default_grace_ms

  @type user_id :: integer()
  @type event :: {:came_online, user_id(), String.t()} | {:went_offline, user_id(), String.t()}
  @type t :: %__MODULE__{
          online: %{user_id() => String.t()},
          pending: %{user_id() => {reference(), reference()}},
          grace_ms: non_neg_integer()
        }

  @spec topic() :: String.t()
  def topic, do: @topic

  @spec grace_ms() :: non_neg_integer()
  def grace_ms, do: Application.get_env(:helios, :presence_grace_ms, @default_grace_ms)

  @spec new(map(), keyword()) :: t()
  def new(presences, opts \\ []) do
    %__MODULE__{
      online: names(presences),
      grace_ms: Keyword.get_lazy(opts, :grace_ms, &grace_ms/0)
    }
  end

  @spec online_users(t()) :: [{user_id(), String.t()}]
  def online_users(%__MODULE__{online: online}), do: Map.to_list(online)

  @spec to_user_id(String.t() | integer()) :: user_id()
  def to_user_id(id) when is_integer(id), do: id
  def to_user_id(id) when is_binary(id), do: String.to_integer(id)

  @spec handle_diff(t(), %{joins: map(), leaves: map()}) :: {t(), [event()]}
  def handle_diff(%__MODULE__{} = tracker, %{joins: joins, leaves: leaves}) do
    tracker =
      leaves
      |> names()
      |> Enum.reduce(tracker, fn {id, _name}, acc -> schedule_check(acc, id) end)

    {tracker, events} =
      joins
      |> names()
      |> Enum.reduce({tracker, []}, fn {id, name}, {acc, events} ->
        case join(acc, id, name) do
          {acc, nil} -> {acc, events}
          {acc, event} -> {acc, [event | events]}
        end
      end)

    {tracker, Enum.reverse(events)}
  end

  @spec confirm_offline(t(), user_id(), reference(), map()) :: {t(), [event()]}
  def confirm_offline(%__MODULE__{} = tracker, user_id, token, presences) do
    case Map.fetch(tracker.pending, user_id) do
      {:ok, {^token, _timer}} ->
        tracker = %{tracker | pending: Map.delete(tracker.pending, user_id)}

        if Map.has_key?(names(presences), user_id) do
          {tracker, []}
        else
          {name, online} = Map.pop(tracker.online, user_id)
          {%{tracker | online: online}, [{:went_offline, user_id, name}]}
        end

      _ ->
        {tracker, []}
    end
  end

  defp join(tracker, id, name) do
    cond do
      Map.has_key?(tracker.pending, id) -> {cancel_check(tracker, id), nil}
      Map.has_key?(tracker.online, id) -> {tracker, nil}
      true -> {%{tracker | online: Map.put(tracker.online, id, name)}, {:came_online, id, name}}
    end
  end

  defp schedule_check(tracker, id) do
    if Map.has_key?(tracker.online, id) do
      tracker = cancel_check(tracker, id)
      token = make_ref()
      timer = Process.send_after(self(), {:confirm_offline, id, token}, tracker.grace_ms)
      %{tracker | pending: Map.put(tracker.pending, id, {token, timer})}
    else
      tracker
    end
  end

  defp cancel_check(tracker, id) do
    case Map.pop(tracker.pending, id) do
      {nil, _pending} ->
        tracker

      {{_token, timer}, pending} ->
        Process.cancel_timer(timer)
        %{tracker | pending: pending}
    end
  end

  defp names(presences) do
    Map.new(presences, fn {key, %{metas: [meta | _]}} -> {to_user_id(key), meta.name} end)
  end
end

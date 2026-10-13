defmodule Helios.AI.Readiness do
  @moduledoc """
  Which allowlisted models may be seated. Every provider is probed at boot,
  every `probe_interval_ms`, and on `refresh/0` (at most once per refresh gap).
  Probes run in a `Helios.AI.TaskSupervisor` task, so `ready?/1` and
  `ready_models/0` never wait on the network. After every probe round
  `{:ai_readiness_changed}` is broadcast on `"ai:readiness"`. A provider whose
  probe fails or raises makes none of its models ready; its reason is logged.
  """
  use GenServer

  require Logger

  alias Helios.AI.Models

  @topic "ai:readiness"
  @default_refresh_gap_ms 60_000
  @probe_now_timeout_ms 30_000

  def topic, do: @topic

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  # Key the child id on the instance name: ExUnit's test supervisor is a
  # static :one_for_one, so several Readiness instances in one test need
  # distinct child ids.
  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, __MODULE__),
      start: {__MODULE__, :start_link, [opts]}
    }
  end

  @spec ready?(term(), GenServer.server()) :: boolean()
  def ready?(id, server \\ __MODULE__), do: MapSet.member?(GenServer.call(server, :ready), id)

  @spec ready_models(GenServer.server()) :: [Models.model()]
  def ready_models(server \\ __MODULE__) do
    {ready, models} = GenServer.call(server, :ready_with_models)
    Enum.filter(models, &MapSet.member?(ready, &1.id))
  end

  @doc "Re-probes unless a probe ran within the refresh gap."
  @spec refresh(GenServer.server()) :: :ok
  def refresh(server \\ __MODULE__), do: GenServer.cast(server, :refresh)

  @doc "Probes now and waits for the result (mix task, tests)."
  @spec probe_now(GenServer.server()) :: %{ready: MapSet.t(String.t()), reasons: map()}
  def probe_now(server \\ __MODULE__),
    do: GenServer.call(server, :probe_now, @probe_now_timeout_ms)

  @impl true
  def init(opts) do
    state = %{
      models: Keyword.get_lazy(opts, :models, &Models.all/0),
      interval_ms:
        Keyword.get_lazy(opts, :interval_ms, fn ->
          Keyword.get(Models.config(), :probe_interval_ms, :timer.minutes(10))
        end),
      refresh_gap_ms: Keyword.get(opts, :refresh_gap_ms, @default_refresh_gap_ms),
      ready: MapSet.new(),
      reasons: %{},
      task: nil,
      last_probe_at: nil,
      waiting: []
    }

    Process.send_after(self(), :tick, state.interval_ms)
    {:ok, state, {:continue, :probe}}
  end

  @impl true
  def handle_continue(:probe, state) do
    state = start_probe(state)
    # Await the boot probe so the first ready set is populated before the
    # server accepts calls; every later probe stays asynchronous.
    {:noreply, await_boot(state)}
  end

  # Selective receive: only the boot task's result (or crash) is handled
  # here; anything else stays queued for the normal GenServer loop.
  defp await_boot(%{task: %Task{ref: ref}} = state) do
    receive do
      {^ref, result} ->
        {:noreply, state} = handle_info({ref, result}, state)
        await_boot(state)

      {:DOWN, ^ref, :process, pid, reason} ->
        {:noreply, state} = handle_info({:DOWN, ref, :process, pid, reason}, state)
        await_boot(state)
    end
  end

  defp await_boot(state), do: state

  @impl true
  def handle_call(:ready, _from, state), do: {:reply, state.ready, state}

  def handle_call(:ready_with_models, _from, state),
    do: {:reply, {state.ready, state.models}, state}

  def handle_call(:probe_now, from, state) do
    state = start_probe(state)
    {:noreply, %{state | waiting: [from | state.waiting]}}
  end

  @impl true
  def handle_cast(:refresh, state) do
    if state.last_probe_at == nil or now() - state.last_probe_at >= state.refresh_gap_ms,
      do: {:noreply, start_probe(state)},
      else: {:noreply, state}
  end

  @impl true
  def handle_info(:tick, state) do
    Process.send_after(self(), :tick, state.interval_ms)
    {:noreply, start_probe(state)}
  end

  def handle_info({ref, {ready, reasons}}, %{task: %Task{ref: ref}} = state) do
    Process.demonitor(ref, [:flush])

    for {provider, reason} <- reasons,
        do: Logger.warning("AI provider #{provider} is not ready: #{inspect(reason)}")

    finish(%{state | ready: ready, reasons: reasons})
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, %{task: %Task{ref: ref}} = state) do
    Logger.error("AI readiness probe crashed: #{inspect(reason)}")
    finish(state)
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp finish(state) do
    result = %{ready: state.ready, reasons: state.reasons}
    Enum.each(state.waiting, &GenServer.reply(&1, result))
    Phoenix.PubSub.broadcast(Helios.PubSub, @topic, {:ai_readiness_changed})
    {:noreply, %{state | task: nil, waiting: []}}
  end

  defp start_probe(%{task: nil, models: models} = state) do
    task = Task.Supervisor.async_nolink(Helios.AI.TaskSupervisor, fn -> probe_all(models) end)
    %{state | task: task, last_probe_at: now()}
  end

  defp start_probe(state), do: state

  # One probe per provider, in parallel; a provider that raises or is not
  # whitelisted contributes a reason instead of ready ids.
  defp probe_all(models) do
    models
    |> Enum.group_by(& &1.provider)
    |> Task.async_stream(fn {provider, group} -> {provider, safe_probe(provider, group)} end,
      timeout: :infinity
    )
    |> Enum.reduce({MapSet.new(), %{}}, fn
      {:ok, {_provider, {:ok, ids}}}, {ready, reasons} ->
        {MapSet.union(ready, MapSet.new(ids)), reasons}

      {:ok, {provider, {:error, reason}}}, {ready, reasons} ->
        {ready, Map.put(reasons, provider, reason)}
    end)
  end

  defp safe_probe(provider, models) do
    Models.provider_module(%{provider: provider}).probe(models)
  rescue
    exception -> {:error, {:exception, Exception.message(exception)}}
  end

  defp now, do: System.monotonic_time(:millisecond)
end

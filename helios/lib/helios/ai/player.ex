defmodule Helios.AI.Player do
  @moduledoc """
  One AI seat of one running game. Its `GameServer` starts it and it stops
  with the server (monitor). It behaves like a browser: after every
  `{:game_updated, _}` it fetches its own view and, when a decision is
  pending that it has not handled yet, asks its model under the model's
  `deadline_ms` (in a `Helios.AI.TaskSupervisor` task, hard stop at deadline +
  #{250} ms). A valid answer is submitted as `:ai`; a timeout, error, invalid
  answer, crashed provider or unready model becomes a uniformly random legal
  move submitted as `:random`. One provider call per decision at most; nothing
  during setup (the owner chooses boards for AI seats).
  """
  use GenServer, restart: :temporary

  require Logger

  alias Helios.AI.{Models, Prompt, Readiness}
  alias Helios.Games
  alias Helios.Games.GameServer

  @slack_ms 250
  @sources [:ai, :random]

  def start_link(opts), do: GenServer.start_link(__MODULE__, opts)

  @doc "The pending decision of the view's player, or nil."
  @spec decision_key(map()) :: {pos_integer(), pos_integer(), atom()} | nil
  def decision_key(%{phase: %{kind: :choosing_cards} = phase, me: me, submitted: submitted}) do
    if List.keyfind(submitted, me, 0) == {me, false}, do: {phase.age, phase.turn, :regular}
  end

  def decision_key(%{phase: %{kind: :extra_turn, extra_turn_player: me} = phase, me: me}),
    do: {phase.age, phase.turn, phase.extra_turn_kind}

  def decision_key(_view), do: nil

  @impl true
  def init(opts) do
    server = Keyword.fetch!(opts, :server)
    game_id = Keyword.fetch!(opts, :game_id)
    Process.monitor(server)
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game_id))

    state = %{
      game_id: game_id,
      server: server,
      actor: {:ai, Keyword.fetch!(opts, :seat)},
      model_id: Keyword.fetch!(opts, :model_id),
      done: nil,
      pending: nil
    }

    {:ok, state, {:continue, :look}}
  end

  @impl true
  def handle_continue(:look, state), do: look(state)

  @impl true
  def handle_info({:game_updated, _seq}, state), do: look(state)
  def handle_info({:game_finished}, state), do: stop(state)
  def handle_info({:game_aborted}, state), do: stop(state)

  def handle_info({:DOWN, _ref, :process, server, _reason}, %{server: server} = state),
    do: stop(state)

  def handle_info({ref, result}, %{pending: %{task: %Task{ref: ref}}} = state) do
    Process.demonitor(ref, [:flush])
    {:noreply, answered(state, result)}
  end

  def handle_info(
        {:DOWN, ref, :process, _pid, reason},
        %{pending: %{task: %Task{ref: ref}}} = state
      ),
      do: {:noreply, answered(state, {:error, {:crashed, reason}})}

  def handle_info({:deadline, ref}, %{pending: %{task: %Task{ref: ref} = task}} = state) do
    Task.shutdown(task, :brutal_kill)
    {:noreply, answered(state, {:error, :timeout})}
  end

  # Late replies, deadlines and exits of decisions already settled.
  def handle_info(_message, state), do: {:noreply, state}

  defp look(%{pending: %{}} = state), do: {:noreply, state}

  defp look(state) do
    case call(fn -> GameServer.view(state.server, state.actor) end) do
      {:ok, %{phase: %{kind: :game_over}}} -> stop(state)
      {:ok, view} -> {:noreply, maybe_decide(state, view)}
      {:error, _reason} -> {:noreply, state}
    end
  end

  defp maybe_decide(state, view) do
    case decision_key(view) do
      nil -> state
      key when key == state.done -> state
      key -> decide(state, view, key)
    end
  end

  # A raise or exit anywhere on the decide path (prompt building, adapter
  # lookup, …) must never kill this process and strand the seat: it becomes a
  # failed decision with reason :exception and falls back to a random move.
  defp decide(state, view, key) do
    prompt = Prompt.build(view)
    started = now()
    meta = %{started: started, usage: %{}}

    case {prompt.moves, ready_model(state.model_id)} do
      {[], _model} ->
        Logger.warning("AI #{state.model_id} in game #{state.game_id} has no legal move")
        %{state | done: key}

      {[only], _model} ->
        submit(state, key, prompt.moves, only, :ai, Map.put(meta, :reason, :single_move))

      {moves, nil} ->
        submit(
          state,
          key,
          moves,
          Enum.random(moves),
          :random,
          Map.put(meta, :reason, :model_unavailable)
        )

      {_moves, model} ->
        provider = Models.provider_module(model)
        deadline = started + model.deadline_ms

        task =
          Task.Supervisor.async_nolink(Helios.AI.TaskSupervisor, fn ->
            provider.decide(model, prompt, deadline)
          end)

        Process.send_after(self(), {:deadline, task.ref}, model.deadline_ms + @slack_ms)
        %{state | pending: %{task: task, key: key, moves: prompt.moves, meta: meta}}
    end
  rescue
    exception ->
      Logger.error(
        "AI #{state.model_id} in game #{state.game_id}: decide raised " <>
          Exception.format(:error, exception, __STACKTRACE__)
      )

      exception_fallback(state, key, view)
  catch
    kind, reason ->
      Logger.error(
        "AI #{state.model_id} in game #{state.game_id}: decide #{kind} (#{inspect(reason)})"
      )

      exception_fallback(state, key, view)
  end

  # The random fallback after a decide-path crash: the prompt that just raised
  # is rebuilt for its moves; if that also fails the decision is given up.
  defp exception_fallback(state, key, view) do
    moves = Prompt.build(view).moves

    if moves == [] do
      %{state | done: key}
    else
      submit(state, key, moves, Enum.random(moves), :random, %{
        started: now(),
        usage: %{},
        reason: :exception
      })
    end
  rescue
    _ -> %{state | done: key}
  end

  defp answered(%{pending: pending} = state, result) do
    state = %{state | pending: nil}
    %{key: key, moves: moves, meta: meta} = pending

    with {:ok, id, usage} <- result,
         %{} = move <- Enum.find(moves, &(&1.id == id)) do
      # A malformed usage (e.g. a scalar) must not crash the seat; the move
      # itself is fine, only the token metadata is dropped.
      usage = if is_map(usage), do: usage, else: %{}
      submit(state, key, moves, move, :ai, %{meta | usage: usage} |> Map.put(:reason, nil))
    else
      nil ->
        submit(
          state,
          key,
          moves,
          Enum.random(moves),
          :random,
          Map.put(meta, :reason, :invalid_move)
        )

      {:error, reason} ->
        submit(state, key, moves, Enum.random(moves), :random, Map.put(meta, :reason, reason))

      other ->
        submit(
          state,
          key,
          moves,
          Enum.random(moves),
          :random,
          Map.put(meta, :reason, {:unexpected_result, other})
        )
    end
  end

  # An AI move the server rejects is replaced by one random move; a rejected
  # random move is logged and the decision is given up (never a loop). Only
  # the sources the Games layer allows ever reach the server.
  defp submit(state, key, moves, move, source, meta) when source in @sources do
    choice = Prompt.to_choice(move)
    result = server_submit(state, choice, source)
    report(state, source, meta, result)

    case {result, source} do
      {:ok, _source} ->
        %{state | done: key}

      {{:error, reason}, :ai} ->
        submit(
          state,
          key,
          moves,
          Enum.random(moves),
          :random,
          Map.put(meta, :reason, {:rejected, reason})
        )

      {{:error, reason}, :random} ->
        Logger.error(
          "AI #{state.model_id} in game #{state.game_id}: random move rejected (#{inspect(reason)})"
        )

        %{state | done: key}
    end
  end

  defp server_submit(state, choice, source),
    do: call(fn -> GameServer.submit_choice(state.server, state.actor, choice, source) end)

  defp report(state, source, meta, result) do
    latency = now() - meta.started

    :telemetry.execute(
      [:helios, :ai, :decision],
      %{
        latency_ms: latency,
        input_tokens: Map.get(meta.usage, :input_tokens, 0),
        output_tokens: Map.get(meta.usage, :output_tokens, 0)
      },
      %{game_id: state.game_id, model_id: state.model_id, source: source, reason: meta[:reason]}
    )

    Logger.info(
      "AI #{state.model_id} in game #{state.game_id}: #{source} move in #{latency} ms " <>
        "(reason #{inspect(meta[:reason])}, tokens #{inspect(meta.usage)}, #{inspect(result)})"
    )
  end

  defp ready_model(model_id) do
    with {:ok, model} <- Models.fetch(model_id),
         true <- Readiness.ready?(model_id) do
      model
    else
      _ -> nil
    end
  end

  defp call(fun) do
    fun.()
  catch
    :exit, reason -> {:error, {:server_exit, reason}}
  end

  defp stop(%{pending: %{task: task}} = state) do
    Task.shutdown(task, :brutal_kill)
    {:stop, :normal, %{state | pending: nil}}
  end

  defp stop(state), do: {:stop, :normal, state}

  defp now, do: System.monotonic_time(:millisecond)
end

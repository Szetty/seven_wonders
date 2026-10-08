defmodule HeliosWeb.GameComponents.PlayerPanels do
  @moduledoc "Shared player widgets: the stats row and the Chosen/Thinking chip."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.Icons, only: [military_icon: 1]

  alias HeliosWeb.GameAssets

  attr :id, :string, required: true
  attr :status, :atom, default: nil

  def status_chip(assigns) do
    ~H"""
    <span
      :if={@status}
      id={@id}
      data-status={@status}
      class={[
        "inline-flex shrink-0 items-center gap-1 rounded-full px-2 py-0.5 text-xs font-semibold",
        status_class(@status)
      ]}
    >
      <.icon name={status_icon(@status)} class="size-3.5" />{status_label(@status)}
    </span>
    """
  end

  defp status_label(:chosen), do: "Chosen"
  defp status_label(:thinking), do: "Thinking…"
  defp status_label(:playing), do: "Playing…"

  defp status_icon(:chosen), do: "hero-check"
  defp status_icon(:thinking), do: "hero-ellipsis-horizontal"
  defp status_icon(:playing), do: "hero-clock"

  defp status_class(:chosen), do: "bg-emerald-100 text-emerald-800"
  defp status_class(:thinking), do: "bg-sky-100 text-sky-800"
  defp status_class(:playing), do: "bg-amber-100 text-amber-800"

  attr :player, :map, required: true

  def player_stats(assigns) do
    ~H"""
    <div class="flex flex-wrap items-center gap-3 text-sm text-zinc-800">
      <span class="flex items-center gap-1" title="Coins">
        <img src={GameAssets.token_path(:coin)} alt="Coins" class="size-5" />
        <span data-stat="coins">{@player.coins}</span>
      </span>
      <span class="flex items-center gap-1" title="Military strength">
        <.military_icon class="size-5" />
        <span data-stat="military">{@player.shields}</span>
      </span>
      <span class="flex items-center gap-1" title="Wonder stages">
        <img src={GameAssets.token_path(:pyramid)} alt="Wonder stages" class="size-5" />
        <span data-stat="stages">{@player.stages_built}/{@player.stages_total}</span>
      </span>
      <span
        :if={@player.military_tokens != []}
        class="flex items-center gap-0.5"
        title="Military tokens"
      >
        <img
          :for={token <- @player.military_tokens}
          src={GameAssets.token_path({:military, token})}
          alt={"Military #{token}"}
          data-token={token}
          class="size-5"
        />
      </span>
    </div>
    """
  end
end

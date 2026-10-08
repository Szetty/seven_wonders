defmodule HeliosWeb.GameComponents.StageStrip do
  @moduledoc "Wonder stage slots along the top of a wonder image; each opens its cost and effect."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.CardInfo, only: [cost_icons: 1]

  alias Helios.Catalogue
  alias HeliosWeb.{CardText, GameAssets, GameFormat}

  attr :id, :string, required: true
  attr :player, :map, required: true
  attr :owner, :string, required: true

  def stage_strip(assigns) do
    ~H"""
    <div
      id={@id}
      class="absolute inset-x-0 top-0 flex justify-center gap-2 bg-linear-to-b from-black/50 to-transparent p-2"
    >
      <%= for n <- 1..@player.stages_total//1 do %>
        <button
          id={"stage-#{@player.name}-#{n}"}
          type="button"
          data-built={to_string(n <= @player.stages_built)}
          aria-haspopup="dialog"
          aria-label={"Stage #{n} of #{@owner}'s wonder"}
          phx-click={JS.toggle(to: "#stage-info-#{@player.name}-#{n}", display: "block")}
          class={[
            "h-12 w-8 overflow-hidden rounded ring-2 transition hover:scale-105 sm:h-16 sm:w-11 pointer-coarse:min-h-11 pointer-coarse:min-w-11",
            if(n <= @player.stages_built, do: "ring-amber-400", else: "bg-black/30 ring-white/60")
          ]}
        >
          <img
            :if={n <= @player.stages_built}
            src={GameAssets.card_back_path(min(n, 3))}
            alt={"Stage #{n} built"}
            class="h-full w-full object-cover"
          />
        </button>
        <.stage_info player={@player} n={n} owner={@owner} />
      <% end %>
    </div>
    """
  end

  attr :player, :map, required: true
  attr :n, :integer, required: true
  attr :owner, :string, required: true

  defp stage_info(assigns) do
    assigns =
      assign(
        assigns,
        :stage,
        Catalogue.stage(assigns.player.wonder, assigns.player.side, assigns.n)
      )

    ~H"""
    <div
      id={"stage-info-#{@player.name}-#{@n}"}
      role="dialog"
      aria-label={"Stage #{@n}"}
      phx-click-away={JS.hide(to: "#stage-info-#{@player.name}-#{@n}")}
      phx-window-keydown={JS.hide(to: "#stage-info-#{@player.name}-#{@n}")}
      phx-key="Escape"
      class="fixed inset-x-4 top-1/3 z-50 mx-auto hidden max-w-xs rounded-xl bg-white p-3 text-left text-sm text-zinc-800 shadow-2xl ring-1 ring-black/10"
    >
      <p class="font-semibold">
        {@player.wonder} {GameFormat.side_label(@player.side)} · Stage {@n}
      </p>
      <p class="text-xs text-zinc-500">
        {@owner} · {if @n <= @player.stages_built, do: "Built", else: "Not built yet"}
      </p>
      <%= if @stage do %>
        <p class="mt-1 flex items-center gap-1">
          <strong>Cost:</strong> <.cost_icons cost={@stage.cost} />
        </p>
        <p :for={line <- CardText.effects(@stage.effects)} class="effect-line">{line}</p>
      <% end %>
    </div>
    """
  end
end

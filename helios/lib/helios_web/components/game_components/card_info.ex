defmodule HeliosWeb.GameComponents.CardInfo do
  @moduledoc "What a card does, what it costs, and how it chains."
  use HeliosWeb, :html

  alias Helios.Catalogue
  alias HeliosWeb.{CardText, GameAssets}

  attr :name, :string, required: true
  attr :neighbours, :map, default: %{}
  attr :built, :list, default: [], doc: "names of the viewer's built cards (for the ✓ on chains)"

  def card_info(assigns) do
    assigns = assign(assigns, :info, Catalogue.card(assigns.name))

    ~H"""
    <div
      :if={@info}
      id="card-info"
      class="rounded-lg bg-stone-50 px-3 py-2 text-sm leading-snug text-zinc-700 ring-1 ring-stone-200"
    >
      <p :for={line <- CardText.effects(@info.effects, @neighbours)} class="effect-line">{line}</p>
      <div class="mt-1 hidden flex-wrap items-center gap-x-3 gap-y-0.5 text-xs text-zinc-600 sm:flex">
        <span id="card-cost" class="flex items-center gap-1">
          <strong>Cost:</strong> <.cost_icons cost={@info.cost} />
        </span>
        <span :if={@info.chains_from != []} id="card-chains-from" class="flex items-center gap-1">
          <strong>Free with:</strong>
          <span :for={from <- @info.chains_from} class="flex items-center gap-0.5">
            {from}
            <.icon
              name={if from in @built, do: "hero-check", else: "hero-x-mark"}
              class={["size-3.5", if(from in @built, do: "text-emerald-600", else: "text-zinc-400")]}
            />
          </span>
        </span>
        <span :if={@info.chains_to != []} id="card-chains-to">
          <strong>Leads to:</strong> {Enum.join(@info.chains_to, ", ")}
        </span>
      </div>
    </div>
    """
  end

  attr :cost, :map, required: true

  def cost_icons(%{cost: %{coins: 0, resources: []}} = assigns), do: ~H"<span>Free</span>"

  def cost_icons(assigns) do
    ~H"""
    <span class="inline-flex flex-wrap items-center gap-0.5" title={HeliosWeb.CardText.cost(@cost)}>
      <%= for {resource, count} <- @cost.resources, _ <- 1..count//1 do %>
        <img src={GameAssets.token_path(resource)} alt={to_string(resource)} class="size-4" />
      <% end %>
      <span :if={@cost.coins > 0} class="inline-flex items-center gap-0.5">
        {@cost.coins}<img src={GameAssets.token_path(:coin)} alt="coins" class="size-4" />
      </span>
    </span>
    """
  end
end

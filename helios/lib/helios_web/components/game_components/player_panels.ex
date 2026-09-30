defmodule HeliosWeb.GameComponents.PlayerPanels do
  @moduledoc "Compact panels for neighbours and other players, plus the shared stats row."
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :id, :string, required: true
  attr :label, :string, required: true
  attr :player, :map, required: true
  attr :names, :map, required: true

  def neighbour_panel(assigns) do
    ~H"""
    <section
      id={@id}
      data-player={@player.name}
      class="min-w-0 flex flex-col gap-2 rounded-xl bg-antique/90 p-3 shadow"
    >
      <header class="flex items-center justify-between gap-2">
        <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
        <span class="truncate font-semibold text-zinc-900">
          {GameFormat.player_name(@names, @player.name)}
        </span>
      </header>
      <img
        src={GameAssets.wonder_path(@player.wonder, @player.side)}
        alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
        class="aspect-[16/5] w-full rounded object-cover"
      />
      <.player_stats player={@player} />
      <div class="flex flex-wrap gap-1">
        <span
          :for={card <- @player.built}
          title={card.name}
          data-card={card.name}
          class={["size-4 rounded-sm ring-1 ring-black/10", GameFormat.category_class(card.category)]}
        >
        </span>
      </div>
    </section>
    """
  end

  attr :players, :list, required: true
  attr :names, :map, required: true

  def others_strip(assigns) do
    ~H"""
    <section id="other-players" class="flex gap-3 overflow-x-auto pb-1">
      <article
        :for={player <- @players}
        id={"player-#{player.name}"}
        class="min-w-56 shrink-0 rounded-xl bg-antique/90 p-3 shadow"
      >
        <header class="mb-1 flex items-center justify-between gap-2">
          <span class="truncate font-semibold">{GameFormat.player_name(@names, player.name)}</span>
          <span class="text-xs text-zinc-600">
            {player.wonder} {GameFormat.side_label(player.side)}
          </span>
        </header>
        <.player_stats player={player} />
        <div class="mt-2 flex flex-wrap gap-1">
          <span
            :for={{category, count} <- GameFormat.category_counts(player.built)}
            data-category={category}
            title={GameFormat.category_label(category)}
            class={[
              "min-w-6 rounded px-1.5 text-center text-xs font-bold text-white",
              GameFormat.category_class(category)
            ]}
          >
            {count}
          </span>
        </div>
      </article>
    </section>
    """
  end

  attr :player, :map, required: true

  def player_stats(assigns) do
    ~H"""
    <div class="flex flex-wrap items-center gap-3 text-sm text-zinc-800">
      <span class="flex items-center gap-1" title="Coins">
        <img src={GameAssets.token_path(:coin)} alt="Coins" class="size-5" />
        <span data-stat="coins">{@player.coins}</span>
      </span>
      <span class="flex items-center gap-1" title="Shields">
        <.icon name="hero-shield-check" class="size-5 text-red-700" />
        <span data-stat="shields">{@player.shields}</span>
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

defmodule HeliosWeb.GameComponents.WonderSetup do
  @moduledoc """
  The setup screen shown while `view.phase.kind == :setup`, instead of the
  table and dock: claim a board and side (`choose_board`) or pick a side of
  your drawn board (`choose_side`), then confirm. All of it is public. The
  engine decides every claim; buttons are only disabled when `view.setup`
  says the action cannot succeed. `me` is the seat being chosen for: the
  viewer, or, for the table owner, one of the AI seats (`acting`).
  """
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :setup, :map, required: true, doc: "view.setup"
  attr :me, :string, required: true

  attr :viewer, :string,
    default: nil,
    doc: "the signed-in player's id (\"You\"); defaults to `me`"

  attr :acting, :list, default: [], doc: "seats the viewer may choose for: [%{id, name}]"
  attr :ai_ids, :any, default: MapSet.new(), doc: "engine ids of AI seats"
  attr :names, :map, required: true
  attr :wonders, :list, required: true, doc: "every base wonder name"
  attr :previews, :map, default: %{}, doc: "wonder name => side shown on a free board"

  def wonder_setup(assigns) do
    mine = Enum.find(assigns.setup.seats, &(&1.player == assigns.me))

    waiting =
      for seat <- assigns.setup.seats,
          !seat.confirmed,
          do: GameFormat.player_name(assigns.names, seat.player)

    assigns =
      assign(assigns, mine: mine, waiting: waiting, viewer: assigns.viewer || assigns.me)

    ~H"""
    <section id="wonder-setup" data-mode={@setup.mode} class="flex flex-1 flex-col gap-3 sm:gap-4">
      <header class="rounded-xl bg-antique/90 p-3 shadow">
        <h2 class="text-lg font-semibold text-zinc-900">
          {if(@setup.mode == :choose_board, do: "Choose your wonder", else: "Choose your side")}
        </h2>
        <p class="text-sm text-zinc-700">
          {if(@setup.mode == :choose_board,
            do: "Tap a board to claim it. You can switch until you confirm.",
            else: "Pick side A or B of your board, then confirm."
          )}
        </p>
      </header>

      <div
        :if={length(@acting) > 1}
        id="setup-as"
        role="group"
        aria-label="Choosing for"
        class="flex flex-wrap gap-2"
      >
        <button
          :for={seat <- @acting}
          id={"setup-as-#{seat.id}"}
          type="button"
          phx-click="setup_as"
          phx-value-seat={seat.id}
          aria-pressed={to_string(seat.id == @me)}
          class={[
            "rounded-lg px-3 py-1.5 text-sm font-semibold ring-1 transition pointer-coarse:min-h-11",
            if(seat.id == @me,
              do: "bg-zinc-900 text-white ring-zinc-900",
              else: "bg-white text-zinc-900 ring-zinc-300 hover:bg-zinc-100"
            )
          ]}
        >
          {seat.name}
        </button>
      </div>

      <div
        :if={@setup.mode == :choose_board}
        id="setup-boards"
        class="grid gap-3 sm:grid-cols-2 lg:grid-cols-3"
      >
        <.board_tile
          :for={wonder <- @wonders}
          wonder={wonder}
          holder={Enum.find(@setup.seats, &(&1.wonder == wonder))}
          me={@me}
          viewer={@viewer}
          names={@names}
          preview={Map.get(@previews, wonder, :a)}
          locked={@mine.confirmed}
        />
      </div>

      <%= if @setup.mode == :choose_side do %>
        <div id="setup-sides" class="grid gap-3 sm:grid-cols-2">
          <button
            :for={side <- [:a, :b]}
            id={"setup-side-#{side}"}
            type="button"
            phx-click="claim_wonder"
            phx-value-wonder={@mine.wonder}
            phx-value-side={side}
            aria-pressed={to_string(@mine.side == side)}
            disabled={@mine.confirmed}
            class={[
              "flex flex-col gap-1 rounded-xl bg-antique/90 p-2 text-left shadow ring-2 transition disabled:cursor-default",
              if(@mine.side == side,
                do: "ring-amber-400",
                else: "ring-transparent hover:ring-zinc-300"
              )
            ]}
          >
            <img
              src={GameAssets.wonder_path(@mine.wonder, side)}
              alt={"#{@mine.wonder} #{GameFormat.side_label(side)}"}
              class="aspect-[16/5] w-full rounded-lg object-cover"
            />
            <span class="text-sm font-semibold text-zinc-900">
              Side {GameFormat.side_label(side)}
            </span>
          </button>
        </div>
        <ul id="setup-seats" class="grid gap-2 rounded-xl bg-antique/90 p-3 shadow sm:grid-cols-2">
          <li
            :for={seat <- @setup.seats}
            :if={seat.player != @me}
            id={"setup-seat-#{seat.player}"}
            class="flex min-w-0 items-center gap-2 text-sm"
          >
            <img
              src={GameAssets.wonder_path(seat.wonder, seat.side || :a)}
              alt=""
              class="aspect-[16/5] w-16 shrink-0 rounded object-cover"
            />
            <span class="truncate font-semibold text-zinc-900">
              {GameFormat.player_name(@names, seat.player)}
            </span>
            <span class="whitespace-nowrap text-zinc-700">
              {seat.wonder} · {if(seat.side,
                do: GameFormat.side_label(seat.side),
                else:
                  if(MapSet.member?(@ai_ids, seat.player),
                    do: "owner is choosing",
                    else: "choosing"
                  )
              )}
            </span>
            <span :if={seat.confirmed} data-confirmed class="shrink-0 text-emerald-600">
              <.icon name="hero-check-circle" class="size-4" />
            </span>
          </li>
        </ul>
      <% end %>

      <div class="sticky bottom-0 z-30 -mx-2 flex flex-wrap items-center justify-end gap-3 rounded-t-xl bg-antique/95 px-3 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] shadow-[0_-4px_12px_rgba(0,0,0,0.2)] sm:mx-0">
        <p :if={@mine.confirmed} id="setup-waiting" class="mr-auto text-sm text-zinc-700">
          Waiting for {Enum.join(@waiting, ", ")}
        </p>
        <button
          id="confirm-wonder"
          type="button"
          phx-click="confirm_wonder"
          disabled={is_nil(@mine.side) or @mine.confirmed}
          class="rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow transition hover:bg-zinc-700 active:bg-zinc-800 disabled:cursor-not-allowed disabled:opacity-50 pointer-coarse:min-h-11"
        >
          {if(@mine.confirmed, do: "Confirmed", else: "Confirm")}
        </button>
      </div>
    </section>
    """
  end

  attr :wonder, :string, required: true
  attr :holder, :map, default: nil
  attr :me, :string, required: true
  attr :viewer, :string, required: true
  attr :names, :map, required: true
  attr :preview, :atom, required: true
  attr :locked, :boolean, required: true

  defp board_tile(assigns) do
    holder = assigns.holder
    mine? = holder != nil and holder.player == assigns.me

    assigns =
      assign(assigns,
        slug: GameAssets.wonder_slug(assigns.wonder),
        mine?: mine?,
        taken?: holder != nil and not mine?,
        side: (holder && holder.side) || assigns.preview
      )

    ~H"""
    <article
      id={"setup-wonder-#{@slug}"}
      data-wonder={@wonder}
      data-side={@side}
      data-holder={@holder && @holder.player}
      data-mine={@mine?}
      class={[
        "flex min-w-0 flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow ring-2",
        if(@mine?, do: "ring-amber-400", else: "ring-transparent")
      ]}
    >
      <button
        id={"claim-#{@slug}"}
        type="button"
        phx-click="claim_wonder"
        phx-value-wonder={@wonder}
        phx-value-side={@side}
        disabled={@holder != nil or @locked}
        aria-label={"Claim #{@wonder} #{GameFormat.side_label(@side)}"}
        class="overflow-hidden rounded-lg transition enabled:hover:-translate-y-0.5 disabled:cursor-default"
      >
        <img
          src={GameAssets.wonder_path(@wonder, @side)}
          alt={"#{@wonder} #{GameFormat.side_label(@side)}"}
          class={["aspect-[16/5] w-full object-cover", @taken? && "opacity-60 grayscale"]}
        />
      </button>
      <div class="flex items-center justify-between gap-2">
        <span class="flex min-w-0 items-center gap-1.5 text-sm">
          <span class="font-semibold text-zinc-900">{@wonder}</span>
          <span :if={@holder} data-holder-name class="truncate text-zinc-700">
            {if(@holder.player == @viewer,
              do: "You",
              else: GameFormat.player_name(@names, @holder.player)
            )}
          </span>
          <span :if={@holder && @holder.confirmed} data-confirmed class="shrink-0 text-emerald-600">
            <.icon name="hero-check-circle" class="size-4" />
          </span>
        </span>
        <span
          role="group"
          aria-label={"#{@wonder} side"}
          class="flex shrink-0 overflow-hidden rounded-md ring-1 ring-zinc-300"
        >
          <button
            :for={side <- [:a, :b]}
            id={"side-#{@slug}-#{side}"}
            type="button"
            phx-click={if(@mine?, do: "claim_wonder", else: "show_side")}
            phx-value-wonder={@wonder}
            phx-value-side={side}
            aria-pressed={to_string(@side == side)}
            disabled={@taken? or @locked}
            class={[
              "px-3 py-1 text-sm font-semibold transition disabled:cursor-not-allowed disabled:opacity-50 pointer-coarse:min-h-11 pointer-coarse:min-w-11",
              if(@side == side,
                do: "bg-zinc-900 text-white",
                else: "bg-white text-zinc-900 hover:bg-zinc-100"
              )
            ]}
          >
            {GameFormat.side_label(side)}
          </button>
        </span>
      </div>
    </article>
    """
  end
end

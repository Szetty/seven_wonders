defmodule HeliosWeb.Layouts do
  @moduledoc """
  This module holds layouts and related functionality
  used by your application.
  """
  use HeliosWeb, :html

  # Embed all files in layouts/* within this module.
  # The default root.html.heex file contains the HTML
  # skeleton of your application, namely HTML headers
  # and other static content.
  embed_templates "layouts/*"

  @doc """
  Renders the app layout: the site header (logged-in users only), a
  full-bleed `<main>` and the flash group.

  ## Examples

      <Layouts.app flash={@flash} current_scope={@current_scope}>
        <h1>Content</h1>
      </Layouts.app>

  """
  attr :flash, :map, required: true, doc: "the map of flash messages"

  attr :current_scope, :map,
    default: nil,
    doc: "the current [scope](https://hexdocs.pm/phoenix/scopes.html)"

  attr :notifications, :list,
    default: [],
    doc: "notifications from HeliosWeb.Notifications (authenticated pages)"

  slot :inner_block, required: true

  def app(assigns) do
    ~H"""
    <div class="min-h-dvh">
      <div :if={@current_scope && @current_scope.user} class="px-2 pt-2 sm:px-6 sm:pt-4 lg:px-8">
        <.site_header current_scope={@current_scope} />
        <.notifications items={@notifications} />
      </div>

      <main class="w-full">
        {render_slot(@inner_block)}
      </main>

      <.flash_group flash={@flash} />
    </div>
    """
  end

  @doc """
  Teal notification strip. Approve notifications carry Accept/Decline; simple
  ones an OK button. Events are handled by `HeliosWeb.Notifications`.
  """
  attr :items, :list, required: true

  def notifications(assigns) do
    ~H"""
    <div
      id="notifications"
      aria-live="polite"
      class="flex flex-col sm:pointer-events-none sm:fixed sm:inset-x-0 sm:top-4 sm:z-50 sm:items-center sm:gap-2 sm:px-4"
    >
      <div
        :for={n <- HeliosWeb.Notifications.visible(@items)}
        id={"notification-#{n.id}"}
        data-notification={Atom.to_string(n.kind)}
        class="pointer-events-auto mt-2 flex w-full flex-wrap items-center justify-between gap-x-4 gap-y-2 rounded-lg bg-teal-600 px-4 py-2 text-white shadow-lg ring-1 ring-teal-900/30 transition sm:mt-0 sm:max-w-xl"
      >
        <span class="min-w-0 flex-1 text-sm font-medium">{n.message}</span>
        <div class="flex shrink-0 gap-2">
          <%= if n.kind == :approve do %>
            <button
              id={"accept-invite-#{n.lobby_id}"}
              type="button"
              phx-click="accept_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-white px-3 py-1 text-sm font-semibold text-teal-800 transition hover:bg-teal-50 active:bg-teal-100 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
            >
              Accept
            </button>
            <button
              id={"decline-invite-#{n.lobby_id}"}
              type="button"
              phx-click="decline_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
            >
              Decline
            </button>
          <% else %>
            <button
              id={"dismiss-#{n.id}"}
              type="button"
              phx-click="dismiss_notification"
              phx-value-id={n.id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
            >
              OK
            </button>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  The rounded teal→blue header bar: username on the left, Logout on the right.
  """
  attr :current_scope, :map, required: true

  def site_header(assigns) do
    ~H"""
    <header
      id="site-header"
      class="flex items-center justify-between gap-2 rounded-2xl bg-linear-to-b from-header-from to-header-to px-3 py-2 text-white shadow-md sm:px-6 sm:py-3"
    >
      <span
        id="current-user-name"
        class="min-w-0 truncate text-base font-semibold tracking-wide sm:text-lg"
      >
        {@current_scope.user.name}
      </span>
      <.link
        href={~p"/"}
        id="my-table-link"
        class="inline-flex shrink-0 items-center justify-center rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        My table
      </.link>
      <.link
        id="logout-link"
        href={~p"/session"}
        method="delete"
        class="inline-flex shrink-0 items-center justify-center rounded-lg px-3 py-1.5 font-medium text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        Logout
      </.link>
    </header>
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} />
  """
  attr :flash, :map, required: true, doc: "the map of flash messages"
  attr :id, :string, default: "flash-group", doc: "the optional id of flash container"

  def flash_group(assigns) do
    ~H"""
    <div
      id={@id}
      aria-live="polite"
      class="fixed inset-x-2 top-2 z-50 flex flex-col gap-2 sm:inset-x-auto sm:top-4 sm:right-4 sm:w-96"
    >
      <.flash kind={:info} flash={@flash} />
      <.flash kind={:error} flash={@flash} />

      <.flash
        id="client-error"
        kind={:error}
        title="We can't find the internet"
        phx-disconnected={show(".phx-client-error #client-error") |> JS.remove_attribute("hidden")}
        phx-connected={hide("#client-error") |> JS.set_attribute({"hidden", ""})}
        hidden
      >
        Attempting to reconnect
        <.icon name="hero-arrow-path" class="ml-1 size-3 motion-safe:animate-spin" />
      </.flash>

      <.flash
        id="server-error"
        kind={:error}
        title="Something went wrong!"
        phx-disconnected={show(".phx-server-error #server-error") |> JS.remove_attribute("hidden")}
        phx-connected={hide("#server-error") |> JS.set_attribute({"hidden", ""})}
        hidden
      >
        Attempting to reconnect
        <.icon name="hero-arrow-path" class="ml-1 size-3 motion-safe:animate-spin" />
      </.flash>
    </div>
    """
  end
end

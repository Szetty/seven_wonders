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
    <div class="min-h-screen">
      <div :if={@current_scope && @current_scope.user} class="px-4 pt-4 sm:px-6 lg:px-8">
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
      class="pointer-events-none fixed inset-x-0 top-4 z-50 flex flex-col items-center gap-2 px-4"
    >
      <div
        :for={n <- HeliosWeb.Notifications.visible(@items)}
        id={"notification-#{n.id}"}
        data-notification={Atom.to_string(n.kind)}
        class="pointer-events-auto flex w-full max-w-xl items-center justify-between gap-4 rounded-lg bg-teal-600 px-4 py-2 text-white shadow-lg ring-1 ring-teal-900/30 transition"
      >
        <span class="text-sm font-medium">{n.message}</span>
        <div class="flex shrink-0 gap-2">
          <%= if n.kind == :approve do %>
            <button
              id={"accept-invite-#{n.lobby_id}"}
              type="button"
              phx-click="accept_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-white px-3 py-1 text-sm font-semibold text-teal-800 transition hover:bg-teal-50"
            >
              Accept
            </button>
            <button
              id={"decline-invite-#{n.lobby_id}"}
              type="button"
              phx-click="decline_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700"
            >
              Decline
            </button>
          <% else %>
            <button
              id={"dismiss-#{n.id}"}
              type="button"
              phx-click="dismiss_notification"
              phx-value-id={n.id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700"
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
      class="flex items-center justify-between rounded-2xl bg-linear-to-b from-header-from to-header-to px-6 py-3 text-white shadow-md"
    >
      <span id="current-user-name" class="text-lg font-semibold tracking-wide">
        {@current_scope.user.name}
      </span>
      <.link
        href={~p"/"}
        id="my-table-link"
        class="rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white"
      >
        My table
      </.link>
      <.link
        id="logout-link"
        href={~p"/session"}
        method="delete"
        class="rounded-lg px-3 py-1.5 font-medium text-white/90 transition hover:bg-white/15 hover:text-white"
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
      class="fixed top-4 right-4 z-50 flex w-80 flex-col gap-2 sm:w-96"
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

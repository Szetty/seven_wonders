defmodule HeliosWeb.LobbyStubLive do
  @moduledoc """
  Phase 1 placeholder for the lobby: greets the logged-in user.
  Replaced by `HeliosWeb.LobbyLive` (`/lobby/:game_id`) in Phase 2.
  """
  use HeliosWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :page_title, "Lobby · 7 Wonders")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="mx-auto max-w-3xl px-4 py-16 text-center sm:px-6 lg:px-8">
        <h1 id="lobby-greeting" class="text-3xl font-bold tracking-tight text-zinc-900">
          Welcome, {@current_scope.user.name}
        </h1>
        <p class="mt-3 text-zinc-600">Your table will appear here soon.</p>
      </section>
    </Layouts.app>
    """
  end
end

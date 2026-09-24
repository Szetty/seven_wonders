defmodule HeliosWeb.LoginLive do
  @moduledoc """
  Login page (`/login`, guests only).

  Validates over the socket with `Helios.Accounts.check_login/2`. When the
  input is valid, it sets `phx-trigger-action` so the browser POSTs the same
  form to `HeliosWeb.UserSessionController`, which writes the session cookie.
  """
  use HeliosWeb, :live_view

  alias Helios.Accounts

  @empty_params %{"access_token" => "", "name" => ""}

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(page_title: "Login · 7 Wonders", error: nil, trigger_submit: false)
     |> assign_form(@empty_params)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <div class="relative flex min-h-screen flex-col items-center justify-center bg-[url(/images/7_wonders.jpg)] bg-cover bg-center px-4 py-12">
        <div class="absolute inset-0 bg-black/30" aria-hidden="true"></div>

        <div class="relative flex w-full max-w-md flex-col items-center">
          <h1
            id="login-title"
            class="mb-8 text-center text-5xl font-bold tracking-widest text-yellow-400 drop-shadow-lg"
          >
            7 WONDERS
          </h1>

          <div
            id="login-card"
            class="w-full rounded-2xl bg-antique px-6 py-8 shadow-2xl ring-1 ring-black/10 sm:px-10"
          >
            <.form
              for={@form}
              id="login_form"
              action={~p"/session"}
              method="post"
              phx-change="validate"
              phx-submit="login"
              phx-trigger-action={@trigger_submit}
              class="space-y-4"
            >
              <.input
                field={@form[:access_token]}
                type="password"
                label="Access Token"
                autocomplete="off"
              />
              <.input field={@form[:name]} type="text" label="Name" autocomplete="username" />
              <.button
                id="login-submit"
                type="submit"
                class="mt-2 w-full"
                phx-disable-with="Entering…"
              >
                Login
              </.button>
            </.form>

            <p
              :if={@error}
              id="login-error"
              role="alert"
              class="mt-4 flex items-center justify-center gap-2 text-sm font-semibold text-rose-700"
            >
              <.icon name="hero-exclamation-circle" class="size-5" />
              {@error}
            </p>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end

  @impl true
  def handle_event("validate", %{"login" => params}, socket) do
    {:noreply, socket |> assign(error: nil) |> assign_form(params)}
  end

  def handle_event("login", %{"login" => params}, socket) do
    case Accounts.check_login(params["access_token"], params["name"]) do
      :ok ->
        {:noreply, socket |> assign(error: nil, trigger_submit: true) |> assign_form(params)}

      {:error, _reason, message} ->
        {:noreply, socket |> assign(error: message) |> assign_form(params)}
    end
  end

  defp assign_form(socket, params), do: assign(socket, :form, to_form(params, as: :login))
end

defmodule HeliosWeb.LoginLive do
  use HeliosWeb, :live_view

  @empty_login %{"name" => "", "access_token" => ""}

  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       form: to_form(@empty_login, as: :login),
       errors: [],
       trigger_submit: false
     )}
  end

  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div
        class="min-h-full flex flex-col items-center justify-center py-12 px-6 lg:px-8 w-full h-screen"
        style="background-image: url('/images/7_wonders.jpg'); background-size: cover; background-position: center;"
      >
        <h1 class="text-5xl font-bold text-center text-yellow-400 p-5">7 WONDERS</h1>
        <div class="mt-8 sm:mx-auto sm:w-full sm:max-w-md">
          <div class="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10">
            <.form
              for={@form}
              id="login_form"
              phx-change="validate"
              phx-submit="login"
              class="space-y-6"
            >
              <div class="md-form md-2">
                <.input
                  field={@form[:access_token]}
                  type="text"
                  label="Access Token"
                  autocomplete="off"
                />
              </div>
              <div class="md-form mt-2">
                <.input
                  field={@form[:name]}
                  type="text"
                  label="Name"
                  autocomplete="username"
                />
              </div>
              <div class="flex justify-center mt-2">
                <.button class="btn btn-outline-dark">
                  Submit
                </.button>
              </div>
            </.form>
            <div id="login_errors" class="mt-2 text-red-600 text-center">
              <div :for={error <- @errors}>{error}</div>
            </div>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end

  def handle_event("validate", %{"login" => login_params}, socket) do
    {:noreply, assign(socket, form: to_form(login_params, as: :login))}
  end

  def handle_event(
        "login",
        %{"login" => %{"name" => name, "access_token" => access_token} = login_params},
        socket
      ) do
    # Placeholder: Phase 1 replaces this with Accounts.check_login/2 + session.
    errors = []
    errors = if String.trim(name) == "", do: ["Name can't be empty!" | errors], else: errors

    errors =
      if String.trim(access_token) == "",
        do: ["Access token can't be empty!" | errors],
        else: errors

    if Enum.empty?(errors) do
      {:noreply,
       socket
       |> put_flash(:info, "Login successful for #{name}!")
       |> assign(form: to_form(@empty_login, as: :login))
       |> assign(errors: [])}
    else
      {:noreply, assign(socket, form: to_form(login_params, as: :login), errors: errors)}
    end
  end
end

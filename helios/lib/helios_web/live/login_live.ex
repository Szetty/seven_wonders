defmodule HeliosWeb.LoginLive do
  use HeliosWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       form: to_form(%{"name" => "", "access_token" => ""}),
       errors: [],
       trigger_submit: false
     )}
  end

  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div class="min-h-full flex flex-col items-center justify-center py-12 px-6 lg:px-8 w-full h-screen" style="background-image: url('/images/7_wonders.jpg'); background-size: cover; background-position: center;">
        <h1 class="text-5xl font-bold text-center text-yellow-400 p-5">7 WONDERS</h1>
        <div class="mt-8 sm:mx-auto sm:w-full sm:max-w-md">
          <div class="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10">
            <.form
              for={@form}
              id="login_form"
              phx-submit="login"
              phx-hook="LoginEnter"
              class="space-y-6"
            >
              <div class="md-form md-2">
                <.input
                  field={@form[:access_token]}
                  type="text"
                  label="Access Token"
                  phx-change="validate"
                  autocomplete="off"
                />
              </div>
              <div class="md-form mt-2">
                <.input
                  field={@form[:name]}
                  type="text"
                  label="Name"
                  phx-change="validate"
                  autocomplete="username"
                />
              </div>
              <div class="flex justify-center mt-2">
                <.button class="btn btn-outline-dark">
                  Submit
                </.button>
              </div>
            </.form>
            <div class="mt-2 text-red-600 text-center">
              <%= for error <- @errors do %>
                <div>{error}</div>
              <% end %>
            </div>
          </div>
        </div>
      </div>
      <script :type={Phoenix.LiveView.ColocatedHook} name=".LoginEnter">
        export default {
          mounted() {
            this.el.addEventListener("keyup", e => {
              if (e.key === "Enter") {
                this.el.dispatchEvent(new Event("submit", {bubbles: true}))
              }
            })
          }
        }
      </script>
    </Layouts.app>
    """
  end

  def handle_event("validate", %{"login_form" => form_params}, socket) do
    form = to_form(form_params)
    {:noreply, assign(socket, form: form)}
  end

  def handle_event(
        "login",
        %{"login_form" => %{"name" => name, "access_token" => access_token} = form_params},
        socket
      ) do
    # Placeholder for validation and login logic
    # In a real application, you would validate the form,
    # call a login service, and handle success/failure.

    # Example validation
    errors = []
    errors = if String.trim(name) == "", do: ["Name can't be empty!" | errors], else: errors

    errors =
      if String.trim(access_token) == "",
        do: ["Access token can't be empty!" | errors],
        else: errors

    if Enum.empty?(errors) do
      # Simulate successful login
      # In a real app, this would involve authenticating and creating a session.
      {:noreply,
       socket
       |> put_flash(:info, "Login successful for #{name}!")
       |> assign(form: to_form(%{"name" => "", "access_token" => ""}))
       |> assign(errors: [])}
    else
      {:noreply, assign(socket, form: to_form(form_params), errors: errors)}
    end
  end
end

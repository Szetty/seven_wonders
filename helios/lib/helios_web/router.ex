defmodule HeliosWeb.Router do
  use HeliosWeb, :router

  import HeliosWeb.UserAuth, only: [fetch_current_scope: 2]

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {HeliosWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_scope
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", HeliosWeb do
    pipe_through :browser

    get "/", PageController, :home
    post "/session", UserSessionController, :create
    delete "/session", UserSessionController, :delete

    live_session :guest, on_mount: [{HeliosWeb.UserAuth, :redirect_if_user}] do
      live "/login", LoginLive, :new
    end

    live_session :authenticated, on_mount: [{HeliosWeb.UserAuth, :require_user}] do
      live "/lobby", LobbyStubLive, :show
    end
  end

  # Enable LiveDashboard in development
  if Application.compile_env(:helios, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: HeliosWeb.Telemetry
    end
  end
end

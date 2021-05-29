defmodule Backend.Crux.Users do
  alias __MODULE__
  use GenServer

  defstruct online_users: []

  # Client

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  # Server (callbacks)

  @impl true
  def init([]) do
    {:ok, %Users{}}
  end
end

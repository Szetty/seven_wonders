defmodule Backend.Crux.Authorizer.Supervisor do
  alias Backend.Crux

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init([]) do
    children = [Crux.Authorizer]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

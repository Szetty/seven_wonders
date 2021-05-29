defmodule Backend.Crux.Supervisor do
  alias Backend.Crux.{Authorizer, Users}
  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init([]) do
    children = [
      Authorizer.Supervisor,
      Users.Supervisor
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

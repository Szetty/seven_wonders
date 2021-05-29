defmodule Backend.Crux.Authorizer do
  alias __MODULE__
  use GenServer

  defmodule User do
    defstruct [:name, :game_id]
  end

  defstruct users: %{}

  # Client

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def create_user(name) do
    GenServer.call(__MODULE__, {:create_user, name})
  end

  def user_name_valid?(name) do
    GenServer.call(__MODULE__, {:user_name_valid?, name})
  end

  # Server (callbacks)

  @impl true
  def init([]) do
    {:ok, %Authorizer{}}
  end

  @impl true
  def handle_call({:create_user, name}, _, %Authorizer{users: users} = authorizer) do
    game_id = UUID.uuid4()

    {reply, users} =
      case Authorizer.Token.create(name, game_id) do
        {:ok, jwt, _} ->
          user = %User{name: name, game_id: game_id}
          users = Map.put(users, name, user)
          reply = {:ok, jwt, user}
          {reply, users}

        error ->
          {error, users}
      end

    {:reply, reply, %{authorizer | users: users}}
  end

  @impl true
  def handle_call({:user_name_valid?, name}, _, %Authorizer{users: users} = authorizer) do
    {:reply, name != "" && !Map.has_key?(users, name), authorizer}
  end
end

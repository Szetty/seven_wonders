defmodule HeliosWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use HeliosWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # The default endpoint for testing
      @endpoint HeliosWeb.Endpoint

      use HeliosWeb, :verified_routes

      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import HeliosWeb.ConnCase
    end
  end

  setup tags do
    Helios.DataCase.setup_sandbox(tags)
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  @doc """
  Setup helper that creates a user and logs them in.

      setup :register_and_log_in_user

  Adds `conn`, `user`, `token` (raw session token) and `scope` to the context.
  """
  def register_and_log_in_user(%{conn: conn}) do
    {:ok, user, token} =
      Helios.Accounts.login(
        Helios.AccountsFixtures.valid_access_token(),
        Helios.AccountsFixtures.unique_user_name()
      )

    %{
      conn: put_user_token(conn, token),
      user: user,
      token: token,
      scope: Helios.Accounts.Scope.for_user(user)
    }
  end

  @doc """
  Logs the given (not currently online) user into `conn` with a fresh session.
  """
  def log_in_user(conn, user) do
    {:ok, _user, token} =
      Helios.Accounts.login(Helios.AccountsFixtures.valid_access_token(), user.name)

    put_user_token(conn, token)
  end

  defp put_user_token(conn, token) do
    conn
    |> Phoenix.ConnTest.init_test_session(%{})
    |> Plug.Conn.put_session(:user_token, token)
  end
end

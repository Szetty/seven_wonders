defmodule BackendWeb.LoginControllerTest do
  alias Backend.Crux.Authorizer.Token
  use BackendWeb.ConnCase
  import OpenApiSpex.TestAssertions

  describe "login" do
    setup do
      Application.put_env(:backend, :access_token, nil)

      on_exit(fn ->
        Application.put_env(:backend, :access_token, nil)
      end)

      :ok
    end

    test "invalid request body", %{conn: conn} do
      resp = call(conn, %{}, 400)
      assert_schema(resp, "ErrorResponse", api_spec())

      assert %{
               "error_message" => "Invalid request body",
               "error_type" => "INVALID_BODY",
               "errors" => [
                 %{
                   "detail" => "Missing field: access_token",
                   "source" => %{"pointer" => "/access_token"}
                 },
                 %{
                   "detail" => "Missing field: name",
                   "source" => %{"pointer" => "/name"}
                 }
               ]
             } = resp
    end

    test "invalid access token", %{conn: conn} do
      body = %{
        "access_token" => "TOKEN",
        "name" => "name"
      }

      resp = call(conn, body, 401)
      assert_schema(resp, "ErrorResponse", api_spec())

      assert %{
               "error_message" => "Wrong access token TOKEN",
               "error_type" => "INVALID_ACCESS_TOKEN",
               "errors" => []
             } = resp
    end

    test "invalid name", %{conn: conn} do
      token = "TOKEN"
      Application.put_env(:backend, :access_token, token)

      body = %{
        "access_token" => token,
        "name" => ""
      }

      resp = call(conn, body, 400)
      assert_schema(resp, "ErrorResponse", api_spec())

      assert %{
               "error_message" => "Name is empty or already taken: ",
               "error_type" => "INVALID_NAME",
               "errors" => []
             } = resp

      name = "valid_name"

      _ = call(conn, %{body | "name" => name}, 200)

      resp = call(conn, %{body | "name" => name}, 400)
      assert_schema(resp, "ErrorResponse", api_spec())

      error_message = "Name is empty or already taken: #{name}"

      assert %{
               "error_message" => ^error_message,
               "error_type" => "INVALID_NAME",
               "errors" => []
             } = resp
    end

    test "valid login", %{conn: conn} do
      token = "TOKEN"
      name = "valid_login_name"
      Application.put_env(:backend, :access_token, token)

      body = %{
        "access_token" => token,
        "name" => name
      }

      resp = call(conn, %{body | "name" => name}, 200)

      assert_schema(resp, "LoginResponse", api_spec())

      assert %{
               "name" => ^name,
               "user_token" => user_token,
               "game_id" => game_id
             } = resp

      game_id_info = UUID.info!(game_id)
      assert 4 = Keyword.get(game_id_info, :version)

      assert {:ok,
              %{
                "gid" => ^game_id,
                "jti" => jti,
                "sub" => ^name
              }} = Token.check(user_token)

      assert jti != ""
    end

    defp call(conn, request, status_code) do
      conn
      |> recycle()
      |> put_req_header("content-type", "application/json")
      |> post(Routes.login_path(conn, :login), request)
      |> json_response(status_code)
    end
  end
end

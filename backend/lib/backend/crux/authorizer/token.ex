defmodule Backend.Crux.Authorizer.Token do
  use Joken.Config

  @impl true
  def token_config do
    default_claims(skip: [:iss, :iat, :nbf, :aud, :exp])
    |> add_claim("sub", nil, &(&1 != ""))
    |> add_claim("gid", nil, &validate_gid/1)
  end

  def create(user_name, game_id) do
    claims = %{
      "sub" => user_name,
      "gid" => game_id
    }

    signer = Joken.Signer.create("HS256", jwt_secret())
    __MODULE__.generate_and_sign(claims, signer)
  end

  def check(jwt_token) do
    signer = Joken.Signer.create("HS256", jwt_secret())
    __MODULE__.verify_and_validate(jwt_token, signer)
  end

  defp validate_gid(gid) do
    r = UUID.info(gid)

    if {:ok, info} = r do
      4 == Keyword.get(info, :version)
    else
      false
    end
  end

  defp jwt_secret do
    Application.get_env(:backend, :access_token) <> Application.get_env(:backend, :jwt_secret)
  end
end

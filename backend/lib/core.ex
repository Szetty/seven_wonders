defmodule Core do
  alias Core.Api
  alias Core.Api.Ping
  alias Core.Api.StartGame
  alias Core.Api.GameSettings

  def ping(name) do
    %Ping.PingRequest{name: name}
    |> Ping.PingRequest.encode()
    |> Api.ping()
    |> decode(Ping.PingReply)
    |> map_if_okay(fn %Ping.PingReply{message: message} -> message end)
  end

  def game_settings() do
    Api.game_settings()
    |> decode(GameSettings.Reply)

    # |> map_if_okay(fn %Ping.PingReply{message: message} -> message end)
  end

  def start_game(players, wonder_sides \\ []) do
    wonder_sides =
      Enum.map(wonder_sides, fn {wonder_name, side_b?} ->
        %StartGame.Request.WonderSide{
          wonder_name: wonder_name,
          is_side_b: side_b?
        }
      end)

    %StartGame.Request{players: players, wonder_sides: wonder_sides}
    |> StartGame.Request.encode()
    |> Api.start_game()
  end

  def debug_game(game_ref) do
    game_ref
    |> Api.debug_game()
    |> map_if_okay(&Jason.decode!/1)
  end

  defp decode({:ok, encoded_response}, response_module) do
    decoded_response =
      encoded_response
      |> :binary.list_to_bin()
      |> response_module.decode()

    {:ok, decoded_response}
  end

  defp decode(other, _), do: other

  defp map_if_okay(term, mapper) do
    case term do
      {:ok, res} -> {:ok, mapper.(res)}
      error -> error
    end
  end
end

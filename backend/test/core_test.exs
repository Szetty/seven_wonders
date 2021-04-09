defmodule CoreTest do
  use ExUnit.Case

  test "communication to core works" do
    Node.start(:"test@127.0.0.1", :longnames)
    path = "/Users/arnold/.local/bin/core-exe"
    name = "Backend"
    Process.register(self(), :p)
    port = Port.open({:spawn_executable, path}, [:binary, args: [UUID.uuid4()]])
    try do
      receive do
        from ->
          send from, Core.Api.Ping.PingRequest.encode(%Core.Api.Ping.PingRequest{name: name})
          receive do
            encoded when is_binary(encoded) ->
              assert %Core.Api.Ping.PingReply{message: "Hello, #{name}"} == Core.Api.Ping.PingReply.decode(encoded)
            a ->
              IO.inspect(a)
          after
            2000 -> flunk("Did not receive message")
          end
      after
        2000 -> flunk("Did not receive message")
      end
    after
      Port.close(port)
    end
  end
end

defmodule Core.Api do
  use Rustler, otp_app: :backend, crate: "core"

  # When your NIF is loaded, it will override this function.
  def ping(_a), do: :erlang.nif_error(:nif_not_loaded)

  # When your NIF is loaded, it will override this function.
  def game_settings(), do: :erlang.nif_error(:nif_not_loaded)

  # When your NIF is loaded, it will override this function.
  def start_game(_a), do: :erlang.nif_error(:nif_not_loaded)

  # When your NIF is loaded, it will override this function.
  def debug_game(_a), do: :erlang.nif_error(:nif_not_loaded)
end

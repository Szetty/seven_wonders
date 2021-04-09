defmodule Core do
  use Rustler, otp_app: :backend, crate: "core"

  # When your NIF is loaded, it will override this function.
  def ping(_a), do: :erlang.nif_error(:nif_not_loaded)
end

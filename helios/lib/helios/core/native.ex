defmodule Helios.Core.Native do
  @moduledoc false
  # Raw NIF bindings to the `seven_wonders_core` crate in ../core.
  # Use `Helios.Core` instead of calling these directly.
  use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"

  def game_settings, do: :erlang.nif_error(:nif_not_loaded)
  def start_game(_players, _wonder_sides), do: :erlang.nif_error(:nif_not_loaded)
  def debug_game(_game), do: :erlang.nif_error(:nif_not_loaded)
end

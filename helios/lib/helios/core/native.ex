defmodule Helios.Core.Native do
  @moduledoc false
  use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"

  def game_settings, do: :erlang.nif_error(:nif_not_loaded)
  def new_game(_players, _wonders, _seed), do: :erlang.nif_error(:nif_not_loaded)
  def submit(_game, _player, _action), do: :erlang.nif_error(:nif_not_loaded)
  def view(_game, _player), do: :erlang.nif_error(:nif_not_loaded)
  def debug_game(_game), do: :erlang.nif_error(:nif_not_loaded)
end

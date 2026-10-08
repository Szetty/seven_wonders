defmodule HeliosWeb.GameComponents.PlayerPanelsTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.PlayerPanels

  test "status chips say Chosen, Thinking… or Playing…" do
    for {status, label} <- [chosen: "Chosen", thinking: "Thinking…", playing: "Playing…"] do
      html = render_component(&PlayerPanels.status_chip/1, id: "status-2", status: status)
      assert attrs(html, "#status-2", "data-status") == [Atom.to_string(status)]
      assert text(html, "#status-2") == label
    end

    assert render_component(&PlayerPanels.status_chip/1, id: "status-2", status: nil) =~ ~r/^\s*$/
  end
end

defmodule HeliosWeb.GameComponents.StageStripTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.StageStrip

  test "every stage is a button opening an explanation" do
    html =
      render_component(&StageStrip.stage_strip/1,
        id: "wonder-stages-2",
        player: SampleViews.player("2"),
        owner: "Bob"
      )

    assert attrs(html, "#wonder-stages-2 button", "id") == ["stage-2-1", "stage-2-2", "stage-2-3"]
    assert attrs(html, "#stage-2-1", "data-built") == ["true"]
    assert attrs(html, "#stage-2-2", "data-built") == ["false"]
    assert [js] = attrs(html, "#stage-2-2", "phx-click")
    assert js =~ "#stage-info-2-2"
    assert attrs(html, "#stage-info-2-2", "role") == ["dialog"]
    assert text(html, "#stage-info-2-2") =~ "Gizah A · Stage 2"
    assert text(html, "#stage-info-2-2") =~ "Not built yet"
    assert text(html, "#stage-info-2-2") =~ "Worth 5 victory points"
    assert text(html, "#stage-info-2-1") =~ "Built"
  end
end

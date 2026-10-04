defmodule HeliosWeb.GameComponents.IconsTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.Icons

  test "the military icon is a decorative inline svg" do
    html = render_component(&Icons.military_icon/1, class: "size-4")
    assert attrs(html, "svg[data-icon='military']", "aria-hidden") == ["true"]
    assert attrs(html, "svg[data-icon='military']", "class") == ["size-4"]
  end
end

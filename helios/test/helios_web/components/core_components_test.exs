defmodule HeliosWeb.CoreComponentsTest do
  use ExUnit.Case, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest
  import HeliosWeb.CoreComponents

  defp classes(html, selector) do
    html
    |> LazyHTML.from_fragment()
    |> LazyHTML.query(selector)
    |> LazyHTML.attribute("class")
    |> List.first()
    |> String.split()
  end

  test "button/1 renders a dark button and appends extra classes" do
    assigns = %{}
    html = rendered_to_string(~H|<.button id="go" class="w-full">Go</.button>|)
    classes = classes(html, "button#go")

    assert "bg-zinc-900" in classes
    assert "text-white" in classes
    assert "hover:bg-zinc-700" in classes
    assert "transition" in classes
    assert "w-full" in classes
    refute "btn" in classes
  end

  test "button/1 renders a link when navigate is given" do
    assigns = %{}
    html = rendered_to_string(~H|<.button id="nav" navigate="/lobby">Lobby</.button>|)
    assert "bg-zinc-900" in classes(html, "a#nav")
  end

  test "input/1 shows errors with plain Tailwind styling" do
    assigns = %{}

    html =
      rendered_to_string(~H"""
      <.input id="name" name="name" value="" label="Name" errors={["is broken"]} />
      """)

    doc = LazyHTML.from_fragment(html)
    assert doc |> LazyHTML.query("p") |> LazyHTML.text() =~ "is broken"

    input_classes = classes(html, "input#name")
    assert "border-rose-500" in input_classes
    refute "input" in input_classes
    refute "input-error" in input_classes
  end

  test "flash/1 renders the message as an alert" do
    assigns = %{}
    html = rendered_to_string(~H|<.flash kind={:error} flash={%{"error" => "Nope"}} />|)
    doc = LazyHTML.from_fragment(html)

    assert doc |> LazyHTML.query("#flash-error[role=alert]") |> LazyHTML.text() =~ "Nope"
    refute "alert" in classes(html, "#flash-error")
  end

  test "table/1 renders rows without daisyUI classes" do
    assigns = %{rows: [%{id: 1, name: "alice"}]}

    html =
      rendered_to_string(~H"""
      <.table id="users" rows={@rows}>
        <:col :let={row} label="Name">{row.name}</:col>
      </.table>
      """)

    doc = LazyHTML.from_fragment(html)
    assert doc |> LazyHTML.query("#users tr td") |> LazyHTML.text() =~ "alice"
    refute "table-zebra" in classes(html, "table")
  end
end

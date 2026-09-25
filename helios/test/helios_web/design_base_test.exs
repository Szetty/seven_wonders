defmodule HeliosWeb.DesignBaseTest do
  use ExUnit.Case, async: true

  test "app.css defines the Helios theme tokens without daisyUI or @apply" do
    css = File.read!("assets/css/app.css")

    refute css =~ ~r/daisy/i
    refute css =~ "@apply"

    for token <- [
          "--color-antique: #faebd7",
          "--color-header-from: rgb(18 147 150)",
          "--color-header-to: rgb(18 147 255)",
          "--color-disconnected: #d6d9dc",
          ~s(--font-sans: "Source Sans Pro", "Trebuchet MS", "Lucida Grande", "Helvetica Neue", sans-serif)
        ] do
      assert css =~ token
    end
  end

  test "no daisyUI vendor files or references remain in lib" do
    assert Path.wildcard("assets/vendor/daisyui*") == []

    offenders =
      "lib/**/*.{ex,heex}"
      |> Path.wildcard()
      |> Enum.filter(&(File.read!(&1) =~ ~r/daisy/i))

    assert offenders == []
  end
end

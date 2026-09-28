defmodule HeliosWeb.HTMLHelpers do
  @moduledoc "LazyHTML shortcuts for component tests."

  def count(html, selector), do: html |> query(selector) |> Enum.count()

  def text(html, selector) do
    html |> query(selector) |> LazyHTML.text() |> String.replace(~r/\s+/, " ") |> String.trim()
  end

  def attrs(html, selector, attribute),
    do: html |> query(selector) |> LazyHTML.attribute(attribute)

  defp query(html, selector), do: html |> LazyHTML.from_fragment() |> LazyHTML.query(selector)
end

defmodule Table do
  @moduledoc """
  Documentation for Table.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Table.hello
      :world

  """
  def hello do
    :world
  end

  defp columns do
    [:c1, :c2]
  end

  defp simple_test_data do
    [
      [c1: "c1 r1", c2: "c2 r1"],
      [c1: "c1 r2-", c2: "c2 r2"]
    ]
  end

  def main(_) do
    Table.Formatter.print_table_for_columns(columns(), simple_test_data())
  end
end

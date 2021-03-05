defmodule Table.FormatterTest do

  use ExUnit.Case
  import ExUnit.CaptureIO
  alias Table.Formatter, as: TF

  def columns do
    [:c1, :c2]
  end

  def simple_test_data do
    [
      [c1: "c1 r1", c2: "c2 r1"],
      [c1: "c1 r2-", c2: "c2 r2"]
    ]
  end

  test "Output is correct" do
    result = capture_io fn ->
      TF.print_table_for_columns(columns(), simple_test_data())
    end

    assert result == """
           c1     | c2\s\s\s
           -------+------
           c1 r1  | c2 r1
           c1 r2- | c2 r2
           """
  end
end
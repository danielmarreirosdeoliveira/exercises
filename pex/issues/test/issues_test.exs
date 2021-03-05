defmodule IssuesTest do
  use ExUnit.Case

  test "the truth" do
    assert Issues.CLI.parse_args('a') == 'abcd'
  end
end

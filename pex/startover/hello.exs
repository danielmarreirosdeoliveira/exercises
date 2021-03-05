defmodule ABC do

  def calc(str) do
    _calc(str,0)
  end

  defp _calc([],nr) do
    nr
  end

  defp _calc([?-|t],nr) do
    nr - _calc(t,0)
  end

  defp _calc([?+|t],nr) do
    nr + _calc(t,0)
  end

  defp _calc([h|t],nr) when h in '0123456789' do
    _calc(t,nr * 10 + h - ?0)
  end
end

ExUnit.start
defmodule AssertionTest do
  use ExUnit.Case, async: true

  test "the truth" do
#    assert(ABC.calc('30-41') == -11)
#    assert(ABC.calc('30+4') == 34)

    refute(ABC.calc('30+4') != 34)
  end
end
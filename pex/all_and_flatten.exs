defmodule MyEnum do

  def all?(list, fun) when is_list(list), do: _all?(list, fun)
  defp _all?([], _), do: true
  defp _all?([h|t], fun) do
    unless fun.(h), do: false, else: _all?(t, fun)
  end

  def flatten(list), do: _flatten(list,[])
  defp _flatten([],acc), do: acc
  defp _flatten([h|t],acc) when is_list(h) do
    _flatten(t,Enum.concat(acc,_flatten(h,[])))
  end
  defp _flatten([h|t],acc) do
    _flatten(t,Enum.concat(acc,[h]))
  end
end

#IO.inspect(MyEnum.all?([2,2,2,4], fn x -> x > 1 end))

#IO.inspect(MyEnum.flatten([1,[2,1,[4,6]],3]))
IO.inspect(MyEnum.flatten([1,[2,[1,[4,6]]],[[1],7]]))

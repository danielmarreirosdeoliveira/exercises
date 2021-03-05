defmodule Table.Formatter do

  def print_table_for_columns(headers, data) do
    with columns = make_columns(headers, data),
         widths = calc_widths(columns),
         format = make_widths_format_string(widths) do

      print_header(headers, format)
      print_separator(widths)
      print_line(columns, format)
    end
  end

  def print_line(columns, format) do
    columns
      |> List.zip
      |> Enum.map(&Tuple.to_list/1)
      |> Enum.each(&puts_one_line(&1, format))
  end

  def puts_one_line(what, format) do
    :io.format(format, what)
  end

  def print_separator(widths) do
    IO.puts (widths |> Enum.map_join("-+-", fn width -> List.duplicate("-", width) end))
  end

  def print_header(headers, format) do
    :io.format(format, headers)
  end

  def make_widths_format_string(widths) do
    (widths |> Enum.map_join(" | ", fn width -> "~-#{width}s" end)) <> "~n"
  end

  def make_columns(headers, data) do
    for header <- headers do
      for row <- data, do: row[header]
    end
  end

  def calc_widths(columns) do
    for column <- columns do
      column |> Enum.map(&String.length/1) |> Enum.max
    end
  end
end

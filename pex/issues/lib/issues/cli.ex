defmodule Issues.CLI do

  def run(argv) do
    IO.puts argv
  end

  def parse_args(argv) do
    parse = OptionParser.parse(argv, switches: [help: :boolean])

    case parse do
      { _, [user, project], _} -> IO.puts "#{user} - #{project}"
    end
  end
end
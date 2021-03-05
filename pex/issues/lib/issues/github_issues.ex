defmodule Issues.GithubIssues do

  def fetch do
    HTTPoison.get("https://api.github.com/repos/danielmarreirosdeoliveira/httParrot/issues",
      [{"User-agent", "Daniel danielmarreirosdeoliveira@googlemail.com"}])
      |> handle_response
  end

  def handle_response({:ok,%{status_code: 200, body: body}}) do
    [h|_] = Poison.Parser.parse!(body)
    IO.puts h["labels_url"]
  end
end
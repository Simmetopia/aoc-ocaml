defmodule AdventOfCode do
  @callback process_input(String.t(), integer) :: any()
  @session_file ".session"
  @year_file ".year"

  def ensure_input(day, parser_module) do
    filename = "day_#{day}_input.txt"

    input =
      if File.exists?(filename) do
        File.read!(filename)
      else
        download_input(day, filename)
        ensure_input(day, parser_module)
      end

    parser_module.process_input(input)
  end

  def test_input(test_input, parser_module) do
    parser_module.process_input(test_input)
  end

  def get_token do
    File.read!(@session_file) |> String.trim()
  end

  def get_year do
    if File.exists?(@year_file) do
      File.read!(@year_file) |> String.trim() |> String.to_integer()
    else
      DateTime.utc_now().year
    end
  end

  def download_input(day, filename) do
    year = get_year()
    token = get_token()

    headers = [
      {"Cookie", "session=#{token}"},
      {"User-Agent", "github.com/fangyi-zhou/advent-of-code-elixir-starter by me+aoc@fangyi.io"}
    ]

    url = "https://adventofcode.com/#{year}/day/#{day}/input"

    case HTTPoison.get(url, headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        File.write!(filename, String.trim(body))

      {:ok, %HTTPoison.Response{status_code: code}} ->
        raise "Unable to get data, status #{code}"

      {:error, %HTTPoison.Error{reason: reason}} ->
        raise "HTTP Error: #{reason}"
    end
  end
end

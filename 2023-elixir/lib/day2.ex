defmodule Day2 do
  @behaviour AdventOfCode.Behaviour

  @impl AdventOfCode.Behaviour
  def process_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_line_2/1)
    |> Enum.reject(&(&1 == :error))
    |> List.foldl(0, &Kernel.+/2)
    |> dbg
  end

  def parse_line(line) do
    [lhs, rhs] = line |> String.split(": ")

    data =
      rhs
      |> String.split(";")
      |> Enum.map(&String.split(&1, ", "))
      |> List.flatten()
      |> Enum.map(&check_valid/1)
      |> Enum.any?(&(&1 == false))

    case data do
      false ->
        "Game " <> x = lhs
        x |> String.to_integer()

      true ->
        :error
    end
  end

  def check_valid(list_game) do
    case list_game
         |> String.trim()
         |> String.split(" ") do
      [x, "red"] ->
        Integer.parse(x) |> elem(0) <= 12

      [x, "blue"] ->
        Integer.parse(x) |> elem(0) <= 14

      [x, "green"] ->
        Integer.parse(x) |> elem(0) <= 13
    end
  end

  def parse_line_2(line) do
    [_lhs, rhs] = line |> String.split(": ")

    rhs
    |> String.split(";")
    |> Enum.map(&String.split(&1, ", "))
    |> List.flatten()
    |> Enum.map(fn x ->
      [count, color] = x |> String.trim() |> String.split(" ")
      {count |> String.to_integer(), color}
    end)
    |> Enum.group_by(fn {_count, color} -> color end)
    |> Enum.map(fn {_color, list} ->
      {count, _} = list |> Enum.max_by(fn {count, _color} -> count end)
       count
    end)
    |> List.foldl(1, &Kernel.*/2)
  end
end

defmodule Day3 do
  @behaviour AdventOfCode.Behaviour

  @impl AdventOfCode.Behaviour
  def process_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, ""))
    |> process()
  end

  def process(matrix) do
    Enum.with_index(matrix)
    |> Enum.reduce([], fn {row, row_index}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {element, col_index}, inner_acc ->
        if is_symbol(element) do
          check_neighbors(matrix, row_index, col_index, inner_acc)
        else
          inner_acc
        end
      end)
    end)
  end

  # ... [other


  defp valid_position?(matrix, row_index, col_index) do
    row_index >= 0 &&
      row_index < Enum.count(matrix) &&
      col_index >= 0 &&
      col_index < matrix |> Enum.count() |> Enum.at(0) |> Enum.count()
  end

  defp check_neighbors(matrix, row_index, col_index, acc) do
    relative_positions = [
      {-1, 0},  # Above
      {1, 0},   # Below
      {-1, -1}, # Diagonally upper-left
      {1, 1}    # Diagonally lower-right
    ]

    results = Enum.reduce(relative_positions, [], fn {dx, dy}, results_acc ->
      new_row_index = row_index + dx
      new_col_index = col_index + dy

      if valid_position?(matrix, new_row_index, new_col_index) do
        neighbor = Enum.at(Enum.at(matrix, new_row_index), new_col_index)
        if is_integer(neighbor) do
          [expand_number(new_row_index, new_col_index) | results_acc]
        else
          results_acc
        end
      else
        results_acc
      end
    end)

    {results, acc ++ results}
  end

  defp is_symbol(element) do
    if(element == ".") do
      false
    else
      is_elem = Integer.parse(element)

      case is_elem do
        {num, _} when num > 0 ->
          false

        :error ->
          true
      end
    end
  end

  def expand_number(list_of_chars, col_of_num) do
    case col_of_num do
      0 ->
        list_of_chars |> Enum.slice(0, 3) |> Enum.join() |> String.to_integer()

      2 ->
        list_of_chars |> Enum.slice(0, 3) |> Enum.join() |> String.to_integer()

      _ ->
        list_of_chars
        |> Enum.slice(col_of_num - 2, 5)
        |> num_from_slice()
    end
  end

  def num_from_slice([]) do
  end

  def num_from_slice([hd | rest]) do
    case hd |> Integer.parse() do
      {num, _} when num > 0 ->
        nums = rest |> Enum.slice(0, 2)

        [hd | nums] |> Enum.join() |> Integer.parse() |> elem(0)

      :error ->
        num_from_slice(rest)
    end
  end
end

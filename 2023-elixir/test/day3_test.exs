defmodule Day3Test do
  use ExUnit.Case, async: true

  alias Day3

  @test_input """
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..
  """
  test "day 1 - test" do
    res = AdventOfCode.test_input(@test_input, Day3)
    dbg(res)
  end
end

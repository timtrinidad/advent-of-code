# https://adventofcode.com/2022/day/4

defmodule DayFour do
  def exec(input) do
    ranges = input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(fn x ->
        # For each line split by command and dash into a 4-elem array, parse to int
        x |> String.split([",", "-"]) |> Enum.map(&String.to_integer/1)
      end)
      |> Enum.reduce({0, 0}, fn [a_min, a_max, b_min, b_max], {num_subset, num_overlap} ->
        is_subset = (a_min >= b_min and a_max <= b_max) or (b_min >= a_min and b_max <= a_max)
        is_overlap = a_max >= b_min and b_max >= a_min
        {num_subset + (if is_subset, do: 1, else: 0), num_overlap + (if is_overlap, do: 1, else: 0)}
      end)
  end
end

{:ok, input} = File.read('2022/04/input.txt')
{num_subset, num_overlap} = DayFour.exec(input)
IO.inspect(num_subset, label: '# Pairs with subsets')
IO.inspect(num_overlap, label: '# Pairs with overlaps')
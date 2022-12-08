# https://adventofcode.com/2022/day/1

defmodule DayOne do
  def getTopThreeGroups(input) do
    # Split into groups
    String.split(input, "\n\n")
    |> Enum.map(fn x ->
      # Split each group by newline, cast to int, sum
      String.trim(x)
      |> String.split
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
    |> Enum.sort
    |> Enum.reverse
    |> Enum.slice(0, 3)
  end
end

{:ok, input} = File.read('2022/01/input.txt')
top = DayOne.getTopThreeGroups(input)
IO.inspect(top, label: "Top Three")
IO.inspect(Enum.sum(top), label: "Top Three Total")


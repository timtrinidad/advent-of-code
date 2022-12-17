import AOC

# https://adventofcode.com/2022/day/1
aoc 2022, 1 do
  def p1(input) do
    top = getTopThreeGroups(input) |> Enum.at(0)
    IO.inspect(top, label: "Top")
  end

  def p2(input) do
    top = getTopThreeGroups(input)
    IO.inspect(Enum.sum(top), label: "Top Three Total")
  end

  def getTopThreeGroups(input) do
    # Split into groups
    String.split(input, "\n\n")
    |> Enum.map(fn x ->
      # Split each group by newline, cast to int, sum
      String.trim(x)
      |> String.split()
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
    |> Enum.sort()
    |> Enum.reverse()
    |> Enum.slice(0, 3)
  end
end

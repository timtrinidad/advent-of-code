import AOC

aoc 2022, 18 do
  def p1(input) do
    blocks = parse_input(input)
    blocks
    |> Enum.reduce(0, fn {x, y, z}, acc ->
      # Determine if each of the current block's neighbors is in the mapset.
      # If not, increment the accumulator for each side.
      exposed =
        [{x + 1, y, z}, {x - 1, y, z}, {x, y + 1, z}, {x, y - 1, z}, {x, y, z + 1}, {x, y, z - 1}]
        |> Enum.map(fn neighbor -> if MapSet.member?(blocks, neighbor), do: 0, else: 1 end)
        |> Enum.sum()
      acc + exposed
    end)
    |> IO.inspect()
  end

  def p2(input) do
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn x ->
      String.split(x, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    end)
    |> MapSet.new()
  end
end

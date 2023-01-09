import AOC

# https://adventofcode.com/2021/day/25
aoc 2021, 25 do
  def p1(input) do
    {cucumbers, dimensions} = parse_input(input)
    emulate_round(cucumbers, dimensions, 0, nil)
  end

  def p2(_) do
    # No part 2
  end

  @doc "Parse the input into a cucumber map of directions keyed by {x, y} and the dimensions of the map"
  def parse_input(input) do
    lines =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn line -> String.graphemes(line) |> Enum.with_index() end)
      |> Enum.with_index()

    cucumbers =
      lines
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> Enum.reduce(acc, fn {char, x}, acc ->
          if char in [">", "v"], do: Map.put(acc, {x, y}, char), else: acc
        end)
      end)

    dimensions = {
      length(elem(Enum.at(lines, 0), 0)),
      length(lines)
    }

    {cucumbers, dimensions}
  end

  @doc "Recursively simulate a single round (of east movers first, south movers second) until no movements happened."
  def emulate_round(_, _, num_rounds, 0), do: num_rounds

  def emulate_round(cucumbers, dimensions = {width, height}, num_rounds, _) do
    # Separate into south and east
    by_dir = cucumbers |> Enum.group_by(&elem(&1, 1))

    # Update the locations of any east-mover which can move, keeping track of how many could move
    {east, num_east} =
      by_dir[">"]
      |> Enum.map_reduce(0, fn {{x, y}, dir}, acc ->
        new_pos = {rem(x + 1, width), y}

        if Map.has_key?(cucumbers, new_pos),
          do: {{{x, y}, dir}, acc},
          else: {{new_pos, dir}, acc + 1}
      end)

    # Updated cucumbers has old south + updated east
    cucumbers = Map.new(east ++ by_dir["v"])

    # Update locations of all south movers and keep track of how many moved
    {south, num_south} =
      by_dir["v"]
      |> Enum.map_reduce(0, fn {{x, y}, dir}, acc ->
        new_pos = {x, rem(y + 1, height)}

        if Map.has_key?(cucumbers, new_pos),
          do: {{{x, y}, dir}, acc},
          else: {{new_pos, dir}, acc + 1}
      end)

    # Updated cucumbers has new south and new east
    cucumbers = Map.new(east ++ south)

    # Recurse
    emulate_round(cucumbers, dimensions, num_rounds + 1, num_east + num_south)
  end
end

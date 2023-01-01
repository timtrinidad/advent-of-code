import AOC

aoc 2021, 9 do
  def p1(input) do
    elevations = parse_input(input)

    find_low_points(elevations) |> Enum.map(fn coord ->
      Map.get(elevations, coord) + 1
    end) |> Enum.sum

  end

  def p2(input) do
  end

  @doc "Parse input into a map of elevations keyed by {x,y} coordinates"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.trim()
      |> String.graphemes()
      |> Enum.with_index(1)
      |> Enum.reduce(acc, fn {elevation, x}, acc ->
        Map.put(acc, {x, y}, String.to_integer(elevation))
      end)
    end)
  end

  @doc "Return a list of coordinates that are local low point"
  def find_low_points(elevations) do
    # For each elevation point
    elevations
    |> Enum.reduce([], fn {{x, y}, elevation}, acc ->
      # Check up, down, right, left neighbors to see if it's lower than the neighbors.
      is_lowest =
        [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
        |> Enum.reduce(true, fn {dx, dy}, acc ->
          acc && elevation < Map.get(elevations, {x + dx, y + dy}, 999)
        end)

      if is_lowest, do: [{x, y} | acc], else: acc
    end)
  end
end

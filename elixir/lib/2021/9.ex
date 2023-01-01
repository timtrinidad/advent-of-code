import AOC

aoc 2021, 9 do
  def p1(input) do
    elevations = parse_input(input)

    # For each low point, get the elevation plus 1 and sum them all
    find_low_points(elevations)
    |> Enum.map(fn coord ->
      Map.get(elevations, coord) + 1
    end)
    |> Enum.sum()
  end

  def p2(input) do
    elevations = parse_input(input)

    # For each low point, find the size of its basin
    low_points =
      find_low_points(elevations)
      |> Enum.map(fn coord ->
        # Map to the size of the basin
        find_basin(elevations, [coord], MapSet.new(), MapSet.new()) |> MapSet.size()
      end)
      # Get the product of the top three
      |> Enum.sort(:desc)
      |> Enum.take(3)
      |> Enum.product()
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
          # true && true if current elevation is lower than neighbor.
          # If neighbor doesn't exist (edge), default to a really high elevation like 999
          acc && elevation < Map.get(elevations, {x + dx, y + dy}, 999)
        end)

      # If it is the lowest, add to list of low points
      if is_lowest, do: [{x, y} | acc], else: acc
    end)
  end

  # Base case - no more nodes to explore in queue
  def find_basin(_, [], _, basin), do: basin

  @doc "DFS to find neighboring points which are part of the basin (not 9)"
  def find_basin(elevations, [{x, y} | next], seen, basin) do
    # Consider all four neighbors to explore. Don't explore again if already seen.
    to_explore =
      [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
      |> Enum.filter(fn coord -> !MapSet.member?(seen, coord) end)

    # Mark current coordinates as seen
    seen = MapSet.put(seen, {x, y})

    {next, seen, basin} =
      cond do
        # If current point is 9, do nothing. Move onto next item in queue.
        Map.get(elevations, {x, y}, 9) == 9 -> {next, seen, basin}
        # If elevation is not 9, add valid unseen neighbors to the queue, and add this point to the basin
        true -> {next ++ to_explore, seen, MapSet.put(basin, {x, y})}
      end

    find_basin(elevations, next, seen, basin)
  end
end

import AOC

# https://adventofcode.com/2022/day/24
aoc 2022, 24 do
  def p1(input) do
    {blizzards, dimensions} = parse_input(input)

    # Cache to keep track of already visited scenarios
    :ets.new(:visited, [:set, :named_table])
    # Cache of blizzard locations for a given round number
    :ets.new(:blizzards, [:set, :named_table])
    # Keep track of the furthest distance encountered so far
    :ets.new(:max, [:set, :named_table])
    bfs(dimensions, [{blizzards, {1, 0}, 0}]) |> IO.inspect()
  end

  def p2(input) do
  end

  @doc "Parse the input into a blizzard mapset and the dimensions of the map"
  def parse_input(input) do
    lines =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn line -> String.graphemes(line) |> Enum.with_index() end)
      |> Enum.with_index()

    blizzards =
      lines
      |> Enum.reduce(MapSet.new(), fn {line, y}, acc ->
        line
        |> Enum.reduce(acc, fn {char, x}, acc ->
          if char in [">", "<", "v", "^"], do: MapSet.put(acc, {x, y, char}), else: acc
        end)
      end)

    dimensions = %{
      height: length(lines) - 2,
      width: length(elem(Enum.at(lines, 0), 0)) - 2
    }

    {blizzards, dimensions}
  end

  @doc "Recursively process BFS queue"
  def bfs(dimensions, queue) do
    # Pop queue
    [{blizzards, {curr_x, curr_y}, num_rounds} | queue] = queue

    # get_max_distance(curr_x, curr_y, num_rounds)

    if {curr_x, curr_y} == {dimensions.width, dimensions.height + 1} do
      # Base case - got to the end position
      IO.inspect("Got to end!")
      num_rounds
    else
      # Calculate the new positions of the blizzards
      # (or retrieve from cache of this round num has been encountered before)
      blizzards =
        if length(cache = :ets.lookup(:blizzards, num_rounds)) > 0 do
          cache |> Enum.at(0) |> elem(1)
        else
          blizzards =
            blizzards
            |> Enum.map(fn {x, y, dir} ->
              new =
                case dir do
                  ">" -> {rem(x, dimensions.width) + 1, y, dir}
                  "<" -> {rem(x + dimensions.width - 2, dimensions.width) + 1, y, dir}
                  "v" -> {x, rem(y, dimensions.height) + 1, dir}
                  "^" -> {x, rem(y + dimensions.height - 2, dimensions.height) + 1, dir}
                end
            end)
            |> MapSet.new()

          :ets.insert(:blizzards, {num_rounds, blizzards})
          blizzards
        end

      occupied_positions = blizzards |> Enum.map(fn {x, y, _} -> {x, y} end) |> MapSet.new()

      # For each possible move (N/S/E/W + stay put), determine if we should explore that position for the next round
      next_moves =
        [{0, 1}, {1, 0}, {0, -1}, {-1, 0}, {0, 0}]
        |> Enum.reduce([], fn {dx, dy}, acc ->
          next_move = {curr_x + dx, curr_y + dy}

          should_move? =
            cond do
              # Currently occupied by a blizzard
              MapSet.member?(occupied_positions, next_move) -> false
              # This scenario already visited - don't visit again
              length(:ets.lookup(:visited, {num_rounds, next_move})) > 0 -> false
              # Start/end positions, always allow (never any blizzards, but outside normal map dimensions)
              next_move == {1, 0} -> true
              next_move == {dimensions.width, dimensions.height + 1} -> true
              # Outside of map
              elem(next_move, 0) > dimensions.width -> false
              elem(next_move, 0) < 1 -> false
              elem(next_move, 1) > dimensions.height -> false
              elem(next_move, 1) < 1 -> false
              # Default case
              true -> true
            end

          if should_move?, do: acc ++ [next_move], else: acc
        end)

      # Mark "to visit" scenarios as visited, add to BFS queue
      next_moves |> Enum.each(fn x -> :ets.insert(:visited, {{num_rounds, x}}) end)
      queue = queue ++ Enum.map(next_moves, fn x -> {blizzards, x, num_rounds + 1} end)

      # Shouldn't ever get here - only get herer if input is unsolvable
      if length(queue) > 0, do: bfs(dimensions, queue), else: num_rounds
    end
  end

  @doc "Render the current blizzards"
  def render(blizzards, dimensions, {curr_x, curr_y}) do
    coordinates =
      blizzards
      |> Enum.reduce(%{}, fn {x, y, dir}, acc ->
        update_in(acc, [{x, y}], fn val -> if val == nil, do: [dir], else: val ++ [dir] end)
      end)

    IO.puts("\n")

    for y <- 1..dimensions.height do
      for x <- 1..dimensions.width do
        vals = get_in(coordinates, [{x, y}])

        cond do
          {x, y} == {curr_x, curr_y} -> IO.write("X")
          vals == nil || length(vals) == 0 -> IO.write(".")
          length(vals) > 1 -> IO.write(length(vals))
          true -> IO.write(Enum.at(vals, 0))
        end
      end

      IO.puts("")
    end
  end

  @doc "Get and store the current max distance travelled based on the current coordinates"
  def get_max_distance(curr_x, curr_y, num_rounds) do
    old_max =
      :ets.lookup(:max, :distance) |> Enum.reduce(0, fn x, acc -> max(elem(x, 1), acc) end)

    curr_max = max(old_max, curr_y - 1 + curr_x - 1)

    if curr_max > old_max,
      do:
        (
          :ets.insert(:max, {:distance, curr_max})
          IO.inspect({curr_max, num_rounds, {curr_x, curr_y}})
        )
  end
end

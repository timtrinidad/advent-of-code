import AOC

aoc 2024, 6 do
  @dirs [{0, -1}, {1, 0}, {0, 1}, {-1, 0}];

  def p1(input) do
    {grid, start} = parse_input(input)
    traverse_map(grid, start, 0, MapSet.new())
    |> elem(1)
    |> MapSet.size
    |> IO.inspect
  end

  def p2(input) do
    {grid, start} = parse_input(input)
    {_, init_pts} = traverse_map(grid, start, 0, MapSet.new())
    init_pts
    |> Enum.map(fn
        ^start -> :ok
        pt -> grid |> Map.replace(pt, ?#) |> traverse_map(start, 0, MapSet.new()) |> elem(0)
    end)
    |> Enum.count(&(&1 == :loop))
    |> IO.inspect

  end

  def parse_input(input) do
    grid = for {line, y} <- input |> String.split("\n", trim: true) |> Enum.with_index,
      {char, x} <- line |> String.to_charlist |> Enum.with_index,
      into: %{},
      do: {{x, y}, char}
    start = grid |> Enum.find(&(elem(&1, 1) == ?^)) |> elem(0)

    {grid, start}
  end

  def traverse_map(grid, {x, y}, dir_idx, visited) do
    {dx, dy} = Enum.at(@dirs, dir_idx)
    next_pos = {x + dx, y + dy}
    visited_key = {x, y, dir_idx}

    cond do
      # We've been at this point and direction before - signify a loop
      MapSet.member?(visited, visited_key) ->
        {:loop, nil}
      # We've gone off the map - return list of visited points
      !Map.has_key?(grid, next_pos) ->
        {:ok, visited |> MapSet.put(visited_key) |> Enum.map(&({elem(&1, 0), elem(&1, 1)})) |> MapSet.new}
      # Turn if we're at a wall
      grid[next_pos] == ?# ->
        traverse_map(grid, {x, y}, rem(dir_idx + 1, 4), visited)
      # Advance forwards
      true ->
        traverse_map(grid, next_pos, dir_idx, MapSet.put(visited, visited_key))
    end
  end

end

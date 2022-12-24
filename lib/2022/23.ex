import AOC

# https://adventofcode.com/2022/day/23
aoc 2022, 23 do
  def p1(input) do
    positions = parse_input(input)
    {positions, _} = do_round(positions, [:north, :south, :west, :east], 10, 1)
    count_empty_tiles(positions) |> IO.inspect()
  end

  def p2(input) do
    positions = parse_input(input)
    {_, num_rounds} = do_round(positions, [:north, :south, :west, :east], -1, 1)
    IO.inspect(num_rounds)
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line -> String.to_charlist(line) |> Enum.with_index() end)
    |> Enum.with_index()
    |> Enum.reduce(MapSet.new(), fn {line, y}, acc ->
      line
      |> Enum.reduce(acc, fn {char, x}, acc ->
        if char == ?#, do: MapSet.put(acc, {x, y}), else: acc
      end)
    end)
  end

  def do_round(positions, directions, max_rounds, round_num) do
    proposed_moves =
      positions
      |> Enum.reduce([], fn pos, acc ->
        proposed_pos =
          if should_move?(positions, pos), do: propose_move(positions, pos, directions), else: pos

        acc ++ [{pos, proposed_pos}]
      end)

    proposed_counts = proposed_moves |> Enum.map(&elem(&1, 1)) |> Enum.frequencies()

    {final_positions, any_move} =
      proposed_moves
      |> Enum.map_reduce(false, fn {orig, proposed}, acc ->
        final_pos = if proposed_counts[proposed] == 1, do: proposed, else: orig
        did_move = final_pos != orig
        {final_pos, acc || did_move}
      end)

    final_positions = MapSet.new(final_positions)

    [dir_head | dir_tail] = directions

    cond do
      max_rounds == round_num -> {positions, round_num}
      !any_move -> {positions, round_num}
      true -> do_round(final_positions, dir_tail ++ [dir_head], max_rounds, round_num + 1)
    end
  end

  def should_move?(positions, {x, y}) do
    [
      {x, y + 1},
      {x + 1, y + 1},
      {x + 1, y},
      {x + 1, y - 1},
      {x, y - 1},
      {x - 1, y - 1},
      {x - 1, y},
      {x - 1, y + 1}
    ]
    |> Enum.reduce(false, fn neighbor, acc ->
      acc || MapSet.member?(positions, neighbor)
    end)
  end

  def propose_move(positions, {x, y} = position, directions) do
    Enum.reduce_while(directions, position, fn direction, acc ->
      to_consider =
        case direction do
          :north -> [{x - 1, y - 1}, {x, y - 1}, {x + 1, y - 1}]
          :south -> [{x - 1, y + 1}, {x, y + 1}, {x + 1, y + 1}]
          :east -> [{x + 1, y - 1}, {x + 1, y}, {x + 1, y + 1}]
          :west -> [{x - 1, y - 1}, {x - 1, y}, {x - 1, y + 1}]
        end

      all_empty =
        to_consider
        |> Enum.reduce(true, fn pos, acc -> acc && !MapSet.member?(positions, pos) end)

      #      IO.inspect(all_empty)
      if all_empty, do: {:halt, Enum.at(to_consider, 1)}, else: {:cont, acc}
    end)
  end

  def render(positions) do
    {xmin, xmax, ymin, ymax} = get_range(positions)

    IO.puts("\n\n")

    for y <- ymin..ymax do
      for x <- xmin..xmax do
        if MapSet.member?(positions, {x, y}), do: IO.write("#"), else: IO.write(".")
      end

      IO.puts("")
    end
  end

  def get_range(positions) do
    xs = positions |> Enum.map(fn {x, _} -> x end)
    {xmin, xmax} = xs |> Enum.min_max()
    ys = positions |> Enum.map(fn {_, y} -> y end)
    {ymin, ymax} = ys |> Enum.min_max()
    {xmin, xmax, ymin, ymax}
  end

  def count_empty_tiles(positions) do
    {xmin, xmax, ymin, ymax} = get_range(positions)
    IO.inspect({xmax - xmin, ymax - ymin})
    (xmax - xmin + 1) * (ymax - ymin + 1) - MapSet.size(positions)
  end
end

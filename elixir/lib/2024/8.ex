import AOC

aoc 2024, 8 do
  def p1(input) do
    input
      |> parse_input
      |> find_all_antinodes(:part1)
      |> MapSet.size
      |> IO.inspect
  end

  def p2(input) do
    input
      |> parse_input
      |> find_all_antinodes(:part2)
      |> MapSet.size
      |> IO.inspect
  end

  def parse_input(input) do
    grid = for {line, y} <- input |> String.split("\n", trim: true) |> Enum.with_index,
      {char, x} <- line |> String.to_charlist |> Enum.with_index,
      into: %{},
      do: {{x, y}, char}
    frequencies = grid
      |> Enum.filter(&(elem(&1, 1) != ?.))
      |> Enum.group_by(&(elem(&1, 1)), &(elem(&1, 0)))
    {grid, frequencies}
  end

  def find_all_antinodes({grid, frequencies}, part_num) do
    for {_, points} <- frequencies,
      i <- 0..length(points)-2,
      j <- i+1..length(points)-1 do
      find_antinodes_for_pair(Enum.at(points, i), Enum.at(points, j), grid, part_num)
    end |> List.flatten |> MapSet.new
  end

  def find_antinodes_for_pair({ax, ay}, {bx, by}, grid, :part1) do
    {dx, dy} = {ax - bx, ay - by}
    [{ax + dx, ay + dy}, {bx - dx, by - dy}] |> Enum.filter(&(Map.has_key?(grid, &1)))
  end

  def find_antinodes_for_pair(a. {ax, ay}, b = {bx, by}, grid, :part2) do
    expand_antinodes(b, {ax - bx, ay - by}, grid) ++ expand_antinodes(a, {bx - ax, by - ay},  grid)
  end

  def expand_antinodes({x, y}, d = {dx, dy}, grid) do
    new_pt = {x + dx, y + dy}
    if Map.has_key?(grid, new_pt),  do: [new_pt | expand_antinodes(new_pt, d, grid)], else: []
  end
end

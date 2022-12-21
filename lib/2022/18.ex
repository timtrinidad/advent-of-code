import AOC

# https://adventofcode.com/2022/day/18
aoc 2022, 18 do
  def p1(input) do
    blocks = parse_input(input)
    blocks |> count_sides_with_no_neighbors |> IO.inspect()
  end

  def p2(input) do
    blocks = parse_input(input)
    num_sides_no_neighbors = blocks |> count_sides_with_no_neighbors
    graph = get_air_graph(blocks)
    num_inner_sides = count_inner_sides(graph, blocks)

    (num_sides_no_neighbors - num_inner_sides) |> IO.inspect()
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

  @doc "Count the number of block sides with no neighbors"
  def count_sides_with_no_neighbors(blocks) do
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
  end

  @doc "Given a set of blocks, create a graph of non-occupied (air) coordinates with other neighboring air coordinates as edges"
  def get_air_graph(blocks) do
    {min_x, max_x} = blocks |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
    {min_y, max_y} = blocks |> Enum.map(&elem(&1, 1)) |> Enum.min_max()
    {min_z, max_z} = blocks |> Enum.map(&elem(&1, 2)) |> Enum.min_max()

    graph = Graph.new()

    graph =
      Enum.reduce((min_x - 1)..(max_x + 1), graph, fn x, acc ->
        Enum.reduce((min_y - 1)..(max_y + 1), acc, fn y, acc ->
          Enum.reduce((min_z - 1)..(max_z + 1), acc, fn z, acc ->
            if MapSet.member?(blocks, {x, y, z}), do: acc, else: Graph.add_vertex(acc, {x, y, z})
          end)
        end)
      end)

    Graph.vertices(graph)
    |> Enum.reduce(graph, fn {x, y, z}, acc ->
      neighbors = [
        {x + 1, y, z},
        {x - 1, y, z},
        {x, y + 1, z},
        {x, y - 1, z},
        {x, y, z + 1},
        {x, y, z - 1}
      ]

      neighbors
      |> Enum.reduce(acc, fn neighbor, acc ->
        if MapSet.member?(blocks, neighbor),
          do: acc,
          else: Graph.add_edge(acc, {x, y, z}, neighbor)
      end)
    end)
  end

  @doc "Given a graph of air, find ones which are not reachable from the inside and sum the number of adjacent blocks"
  def count_inner_sides(graph, blocks) do
    outer_blocks = Graph.reachable(graph, [{0, 0, 0}]) |> MapSet.new()

    Graph.vertices(graph)
    |> Enum.filter(fn x -> !MapSet.member?(outer_blocks, x) end)
    |> Enum.reduce(0, fn {x, y, z}, acc ->
      neighbors = [
        {x + 1, y, z},
        {x - 1, y, z},
        {x, y + 1, z},
        {x, y - 1, z},
        {x, y, z + 1},
        {x, y, z - 1}
      ]

      neighbors
      |> Enum.reduce(acc, fn neighbor, acc ->
        if MapSet.member?(blocks, neighbor), do: acc + 1, else: acc
      end)
    end)
  end
end

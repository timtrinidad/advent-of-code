import AOC

# https://adventofcode.com/2021/day/15
aoc 2021, 15 do
  def p1(input) do
    parse_input(input) |> find_lowest_risk_path
  end

  def p2(input) do
    map = parse_input(input)
    map_width = get_map_width(map)

    # Duplicate the map 4 more times in each direction
    map
    |> Enum.flat_map(fn {{x, y}, val} ->
      for dx <- 0..4, dy <- 0..4 do
        # Increase val each time, wrapping from 9 -> 1
        {{x + dx * map_width, y + dy * map_width}, rem(val - 1 + dx + dy, 9) + 1}
      end
    end)
    |> Map.new()
    |> find_lowest_risk_path
  end

  @doc "Parse the input into a map of risks, keyed by {x, y} coordinates"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {val, x}, acc ->
        Map.put(acc, {x, y}, String.to_integer(val))
      end)
    end)
  end

  def find_lowest_risk_path(map) do
    map_width = get_map_width(map)

    # Create a weighted graph where each point has an edge to all neighboring points, weighted by risk
    # Prevent vertex ID collisions: https://github.com/bitwalker/libgraph/issues/44
    graph = Graph.new(vertex_identifier: fn v -> v end)

    graph =
      map
      |> Enum.reduce(graph, fn {{x, y}, val}, acc ->
        new_edges =
          [{x + 1, y}, {x, y + 1}, {x - 1, y}, {x, y - 1}]
          |> Enum.filter(&Map.has_key?(map, &1))
          |> Enum.map(fn neighbor -> {neighbor, {x, y}, weight: val} end)

        Graph.add_edges(acc, new_edges)
      end)

    #    for x <- 0..map_width-1, y <- 0..map_width-1 do
    #      if !Graph.has_vertex?(graph, {x, y}), do: IO.inspect({x, y}, label: "Missing")
    #    end |> length |> IO.inspect
    IO.inspect(graph)

    # Dijkstra from start to finish. Sum risk values of all points in the "shortest" path
    (Graph.dijkstra(graph, {0, 0}, {map_width - 1, map_width - 1})
     |> Enum.map(fn point -> map[point] end)
     |> Enum.sum()) - map[{0, 0}]
  end

  def get_map_width(map) do
    (Map.keys(map) |> Enum.map(&elem(&1, 0)) |> Enum.max()) + 1
  end
end

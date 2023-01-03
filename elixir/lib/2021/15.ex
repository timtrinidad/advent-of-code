import AOC

# https://adventofcode.com/2021/day/15
aoc 2021, 15 do
  def p1(input) do
    map = parse_input(input)
    # Determine the max x value - since we know it's a square, it's also the max y val
    max = Map.keys(map) |> Enum.map(&elem(&1, 0)) |> Enum.max()

    # Create a weighted graph where each point has an edge to all neighboring points, weighted by risk
    graph =
      map
      |> Enum.reduce(Graph.new(), fn {{x, y}, val}, acc ->
        new_edges =
          [{x + 1, y}, {x, y + 1}, {x - 1, y}, {x, y - 1}]
          |> Enum.filter(&Map.has_key?(map, &1))
          |> Enum.map(fn neighbor -> {neighbor, {x, y}, weight: val} end)

        Graph.add_edges(acc, new_edges)
      end)

    # Dijkstra from start to finish. Sum risk values of all points in the "shortest" path
    (Graph.dijkstra(graph, {0, 0}, {max, max})
     |> Enum.map(fn point -> map[point] end)
     |> Enum.sum()) - map[{0, 0}]
  end

  def p2(input) do
    parse_input(input)
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
end

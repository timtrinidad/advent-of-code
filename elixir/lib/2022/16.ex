import AOC

# https://adventofcode.com/2022/day/16
aoc 2022, 16 do
  def p1(input) do
    parse_input(input) |> exec(30) |> IO.inspect()
  end

  def p2(input) do
    valves = parse_input(input)
    closed_valves = valves |> filter_open(false) |> Map.keys()
    half_num_closed = div(length(closed_valves), 2)

    # Find all the combinations for half of the closed valves
    # (i.e. all the combinations for which valves to assign to 1 of 2 actors)
    combinations(half_num_closed, closed_valves)
    |> Enum.map(fn to_open_a ->
      # For each combination, generate a set of valves with those which can only be acted on by actor 1 and another
      # set which can only be acted on by actor 2
      valves_a =
        to_open_a |> Enum.reduce(valves, fn x, valves -> put_in(valves, [x, :open], true) end)

      to_open_b = MapSet.difference(MapSet.new(closed_valves), MapSet.new(to_open_a))

      valves_b =
        to_open_b |> Enum.reduce(valves, fn x, valves -> put_in(valves, [x, :open], true) end)

      # Sum the two simulations
      exec(valves_a, 26) + exec(valves_b, 26)
    end)
    # Find the max of all simulations
    |> Enum.max()
    |> IO.inspect()
  end

  def exec(valves, minutes) do
    graph =
      Graph.new()
      |> Graph.add_vertices(Enum.map(valves, fn {valve, _} -> valve end))
      |> Graph.add_edges(
        Enum.flat_map(valves, fn {valve, %{:neighbors => neighbors}} ->
          Enum.map(neighbors, fn neighbor -> {valve, neighbor} end)
        end)
      )

    cycle(valves, graph, "AA", minutes, 0)
  end

  @doc "Recurse through minutes to open valves until we hit max minutes"
  def cycle(valves, graph, curr, minute, total_pressure) do
    next_valves = valves |> filter_open(false) |> Map.keys()

    cond do
      minute <= 1 || length(next_valves) == 0 ->
        total_pressure

      true ->
        next_valves
        |> Enum.map(fn next_valve ->
          num_hops = get_shortest_path(graph, curr, next_valve)
          new_minute = minute - num_hops

          new_total_pressure = total_pressure + (minute - num_hops) * valves[next_valve][:rate]

          if new_minute > 0 do
            cycle(
              put_in(valves[next_valve][:open], true),
              graph,
              next_valve,
              new_minute,
              new_total_pressure
            )
          else
            total_pressure
          end
        end)
        |> Enum.max()
    end
  end

  @doc "Find and cache the shortest path between two valves"
  def get_shortest_path(graph, from, to) do
    cache = :ets.lookup(:cache, {from, to})

    if length(cache) > 0 do
      cache |> Enum.at(0) |> elem(1)
    else
      hops = Graph.get_shortest_path(graph, from, to) |> length
      :ets.insert(:cache, {{from, to}, hops})
      hops
    end
  end

  @doc "Filter the valves for only those that are open (open=true) or closed (open=false)"
  def filter_open(valves, open),
    do: valves |> Map.filter(fn {_, %{:open => is_open}} -> open == is_open end)

  @doc "Parse the input into an Elixir Map"
  def parse_input(input) do
    :ets.new(:cache, [:set, :named_table])

    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      [valve, flow_rate, out_neighbors] =
        Regex.run(~r/Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/, line)
        |> tl

      flow_rate = flow_rate |> String.to_integer()
      out_neighbors = out_neighbors |> String.split(", ")
      Map.put(acc, valve, %{rate: flow_rate, neighbors: out_neighbors, open: flow_rate == 0})
    end)
  end

  @doc "From https://elixirforum.com/t/generate-all-combinations-having-a-fixed-array-size/26196/7"
  def combinations(0, _), do: [[]]
  def combinations(_, []), do: []

  def combinations(size, [head | tail]) do
    for(elem <- combinations(size - 1, tail), do: [head | elem]) ++ combinations(size, tail)
  end
end

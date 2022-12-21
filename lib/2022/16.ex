import AOC

# https://adventofcode.com/2022/day/16
aoc 2022, 16 do
  def p1(input) do
    parse_input(input) |> exec(30) |> IO.inspect()
  end

  def p2(_) do
  end

  def exec(valves, minutes) do
    :ets.new(:cache, [:set, :named_table])
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
    open_pressure =
      valves
      |> filter_open(true)
      |> Enum.map(fn {_, %{:rate => rate}} -> rate end)
      |> Enum.sum()

    next_options =
      valves
      |> filter_open(false)
      |> Map.filter(fn {valve, _} -> valve != curr end)
      |> Enum.map(fn {valve, _} ->
        num_hops = get_shortest_path(graph, curr, valve)
        {valve, num_hops - 1}
      end)
      |> Enum.filter(fn {_, num_hops} -> minute - num_hops > 1 end)

    cond do
      minute <= 1 ->
        total_pressure

      length(next_options) == 0 ->
        cycle(valves, graph, curr, minute - 1, total_pressure + open_pressure)

      true ->
        next_options
        |> Enum.map(fn next_option ->
          {next_valve, num_hops} = next_option
          minute = minute - num_hops - 1

          total_pressure =
            total_pressure + open_pressure * (num_hops + 1) + valves[next_valve][:rate]

          cycle(
            put_in(valves[next_valve][:open], true),
            graph,
            next_valve,
            minute,
            total_pressure
          )
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
end

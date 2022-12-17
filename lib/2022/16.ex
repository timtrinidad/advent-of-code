import AOC

aoc 2022, 16 do
  def p1(input) do
    valves = parse_input(input)

    graph =
      Graph.new()
      |> Graph.add_vertices(Enum.map(valves, fn {valve, _} -> valve end))
      |> Graph.add_edges(
        Enum.flat_map(valves, fn {valve, %{:neighbors => neighbors}} ->
          Enum.map(neighbors, fn neighbor -> {valve, neighbor} end)
        end)
      )

    valves_by_flow =
      valves |> Enum.sort(fn {_, %{:rate => a}}, {_, %{:rate => b}} -> a > b end) |> IO.inspect()

    cycle(valves, graph, "AA", 1, 0) |> IO.inspect()
  end

  def cycle(valves, graph, curr, minute, total_pressure) do
    #    IO.puts("")
    #    IO.inspect(minute, label: "Minute")
    #    IO.inspect(curr, label: "Current valve")
    open_pressure =
      valves
      |> filter_open(true)
      |> Enum.map(fn {_, %{:rate => rate}} -> rate end)
      |> Enum.sum()

    #      |> IO.inspect(label: "Open pressure")
    #    IO.inspect(total_pressure, label: "Total pressure")

    curr_valve = valves[curr]

    next_options =
      valves
      |> filter_open(false)
      |> Map.filter(fn {valve, _} -> valve != curr end)
      |> Enum.map(fn {valve, %{:rate => rate}} ->
        hops = Graph.get_shortest_path(graph, curr, valve)
        num_hops = hops |> length
        {valve, rate / (num_hops - 1), num_hops - 1, hops}
      end)
      |> Enum.sort(fn {_, a, _, _}, {_, b, _, _} -> a <= b end)
      |> Enum.filter(fn {_, _, num_hops, _} -> minute + num_hops < 30 end)

    cond do
      minute >= 30 ->
        total_pressure

      length(next_options) == 0 ->
        cycle(valves, graph, curr, minute + 1, total_pressure + open_pressure)

      true ->
        next_options
        |> Enum.map(fn next_option ->
          {next_valve, _, num_hops, hops} = next_option
          #          IO.inspect(hops, label: "Next valve")
          minute = minute + num_hops + 1

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

    #    cond do
    #      minute == 20 -> open_pressure
    #      true -> curr_valve.neighbors |> Enum.map(fn neighbor ->
    #          IO.inspect(neighbor)
    #          open_pressure + cycle(valves, graph, neighbor, minute + 1, stack ++ [neighbor])
    #        end) |> Enum.max
    #    end
  end

  def filter_open(valves, open),
    do: valves |> Map.filter(fn {_, %{:open => is_open}} -> open == is_open end)

  def p2(input) do
  end

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

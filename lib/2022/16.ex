import AOC

aoc 2022, 16 do
  def p1(input) do
    valves = parse_input(input)

    :ets.new(:cache, [:set, :protected, :named_table])
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
        {valve, rate / (num_hops - 1), num_hops - 1}
      end)
      |> Enum.sort(fn {_, a, _}, {_, b, _} -> a <= b end)
      |> Enum.filter(fn {_, _, num_hops} -> minute + num_hops < 30 end)

    cache_key = [valves |> filter_open(false) |> Map.keys() |> Enum.sort()|> Enum.join(""), curr, minute] |> Enum.join("|")

    {is_cached, total_pressure} = cond do
      minute >= 30 ->
        {false, total_pressure}

      length(next_options) == 0 ->
    {false, cycle(valves, graph, curr, minute + 1, total_pressure + open_pressure)}

      (cache = :ets.lookup(:cache, cache_key) |> Enum.at(0)) != nil ->
#        IO.inspect(cache, label: "Cache hit")
        {true, cache |> elem(1)}


      true ->
        val = next_options
        |> Enum.map(fn next_option ->
          {next_valve, _, num_hops} = next_option
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
        {false, val}
    end

    if !is_cached &&  length(next_options) > 0, do: :ets.insert(:cache, {cache_key, total_pressure})
    total_pressure
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

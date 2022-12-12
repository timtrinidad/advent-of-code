# https://adventofcode.com/2022/day/12

defmodule DayTwelve do
  def exec(input) do
    map = parse_input(input)
    analyze_map(map)
  end

  def parse_input(input) do
    input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(&String.to_charlist/1)
  end

  def analyze_map(map) do
    analysis = %{
      :start_xy => nil,
      :end_xy => nil,
      :graph => [],
    }
    nodes = map |> Enum.with_index |> Enum.map(fn {row, y} ->
      row |> Enum.with_index |> Enum.map(fn {node, x} ->
        is_start = node == ?S
        is_end = node == ?E
        neighbors = Enum.reduce([{1, 0}, {0, 1}, {-1, 0}, {0, -1}], [], fn {x, y}, acc ->
          neighbor = map |> Enum.at(y) |> Enum.at(x) 
        end
        %{
          :x => x,
          :y => y,
          :is_start => is_start,
          :is_end => is_end,
        }
      end)
    end) |> Enum.concat
    start_point = Enum.find(nodes, fn x -> x.is_start end)
    IO.inspect(start_point)
    end_point = Enum.find(nodes, fn x -> x.is_end end)
    IO.inspect(end_point)
#    map |> Enum.with_index |> Enum.reduce(analysis, fn {row, y}, analysis ->
#
#      start_x = row |> Enum.find_index(fn x -> x == ?S end)
#      analysis = update_in(analysis.start_xy, fn x -> if start_x, do: {start_x, y}, else: x end)
#      end_x = row |> Enum.find_index(fn x -> x == ?E end)
#      analysis = update_in(analysis.end_xy, fn x -> if end_x, do: {end_x, y}, else: x end)
#
#
#      analysis
#    end)
  end
end

defmodule Graph do
  @moduledoc "From https://github.com/applicake/elixir-of-dijkstra/blob/master/graph.ex"

  def dijkstra(configuration, point) do
    g = Dict.get(configuration, :g)
    d = Dict.get(configuration, :d)

    q = Keyword.keys(d)
    d = Keyword.merge(d, [{point, 0}])
    loopy(g, q, d)
  end

  def extract_min([], _d, key) do
    key
  end

  def extract_min([h|t], d, key) do
    if d[h] < d[key] do
      extract_min(t, d, h)
    else
      extract_min(t, d, key)
    end
  end

  def extract_min(q, d) do
    [f|_] = q
    extract_min(q, d, f)
  end

  def relax(_u, [], d) do
    d
  end

  def relax(u, list, d) do
    [{v, w} | t] = list
    if d[v] > d[u] + w do
      d = Keyword.merge(d, [{v, d[u] + w}])
    end
    relax(u, t, d)
  end

  def loopy(g, q, d) do
    if length(q) > 0 do
      d = inloop(g, q, d)
    end
    d
  end

  def inloop(g, q, d) do
    u = extract_min(q, d)
    q = List.delete(q, u)
    d = relax(u, g[u], d)
    d = loopy(g, q, d)
  end
end

{:ok, input} = File.read('2022/12/input.txt')

DayTwelve.exec(input)
  |> IO.inspect(charlists: :as_lists)


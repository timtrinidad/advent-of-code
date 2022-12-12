# https://adventofcode.com/2022/day/12

defmodule DayTwelve do
  def get_paths(input) do
    graph = :digraph.new()
    # Parse input
    elevation_map = input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(&String.to_charlist/1)

    # Initialize vertices, store in a map/dict
    height = length(elevation_map)
    width = length(elevation_map |> Enum.at(0))
    vertices = Enum.flat_map(1..height, fn y -> Enum.map(1..width, fn x -> coord_to_key({x-1, y-1}) end) end)
      |> Enum.reduce(%{}, fn point, acc -> Map.put(acc, point, :digraph.add_vertex(graph, point)) end)

    # Generate all edges
    nodes = elevation_map |> Enum.with_index |> Enum.flat_map(fn {row, y} ->
      row |> Enum.with_index |> Enum.map(fn {node, x} ->
        # Look right, down, left, up. If valid neighbor (not edge, max 1 height diff), record edge.
        nodes = Enum.reduce([{1, 0}, {0, 1}, {-1, 0}, {0, -1}], [], fn {delta_x, delta_y}, acc ->
          neighbor_xy = {x - delta_x, y - delta_y}
          neighbor = (elevation_map |> Enum.at(y - delta_y) || []) |> Enum.at(x - delta_x)
          valid_neighbor = neighbor && elem(neighbor_xy, 0) >= 0 && elem(neighbor_xy, 1) >= 0 && (height(neighbor) - height(node) <= 1)
          if valid_neighbor, do: :digraph.add_edge(graph, vertices[coord_to_key({x, y})], vertices[coord_to_key(neighbor_xy)])
          {node, {x, y}}
        end)
      end)
    end)

    # Get end coordinates
    {_, end_xy} = Enum.find(nodes, fn {node, _} -> node == ?E end)

    # Calculate all paths from any node of height "a" to end
    nodes
      |> Enum.filter(fn {node, _} -> height(node) == ?a end)
      |> Enum.map(fn {node, node_xy} ->
        steps = :digraph.get_short_path(graph, vertices[coord_to_key(node_xy)], vertices[coord_to_key(end_xy)])
        {node, (if steps, do: length(steps)-1, else: nil)}
      end)

  end
  def coord_to_key({x, y}),  do: "#{x},#{y}"
  def height(?S), do: ?a
  def height(?E), do: ?z
  def height(node), do: node
end

{:ok, input} = File.read('2022/12/input.txt')

paths = DayTwelve.get_paths(input)
# Find only path from node "S"
paths
  |> Enum.filter(&match?({?S, _}, &1))
  |> Enum.at(0)
  |> elem(1)
  |> IO.inspect(label: "Shortest path from 'S'")
# Filter out nil paths, sort, get top
paths
  |> Enum.filter(fn {_, steps} -> steps != nil end)
  |> Enum.map(fn {_, steps} -> steps end)
  |> Enum.sort
  |> Enum.at(0) 
  |> IO.inspect(label: "Shortest path from any 'a' or 'S'")
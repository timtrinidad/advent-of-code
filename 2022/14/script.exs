# https://adventofcode.com/2022/day/14

defmodule DayFourteen do
  def count_sand(input, include_floor) do
    wall_paths = input
    |> String.trim
    |> String.split("\n")
    |> Enum.map(fn x -> x |> String.split(" -> ") |> Enum.map(fn x -> x |> String.split(",") |> Enum.map(&String.to_integer/1) |> List.to_tuple end) end)

    {_, x_max, _, y_max} = wall_paths |> Enum.concat() |> Enum.reduce({nil, nil, nil, nil}, fn {x, y}, {x_min, x_max, y_min, y_max} ->
      x_min = if x_min == nil, do: x, else: Enum.min([x, x_min])
      x_max = if x_max == nil, do: x, else: Enum.max([x, x_max])
      y_min = if y_min == nil, do: y, else: Enum.min([y, y_min])
      y_max = if y_max == nil, do: y, else: Enum.max([y, y_max])
      {x_min, x_max, y_min, y_max}
    end)
    y_min = 0
    y_max = y_max + 2
    x_min = -5000
    x_max = x_max + 5000
    map = Enum.reduce(y_min..y_max, %{}, fn y, acc ->
      Enum.reduce(x_min..x_max, acc, fn x, acc ->
        row = put_in((if acc[y] == nil, do: %{}, else: acc[y]), [x], ".")
        put_in(acc, [y], row)
      end)
    end)
    map = if include_floor do
      Enum.reduce(x_min..x_max, map, fn x, acc ->
        put_in(acc, [y_max, x], "#")
      end)
    else
      map
    end

    wall_points = wall_paths
      |> Enum.map(fn [first_point | path] ->
        path |> Enum.flat_map_reduce(first_point, fn {point_x, point_y}, {previous_x, previous_y} ->
          points = for x <- previous_x..point_x do
            for y <- previous_y..point_y, do: {x, y}
          end |> Enum.concat
          {points, {point_x, point_y}}
        end) |> elem(0)
      end) |> Enum.concat

    map = wall_points
      |> Enum.reduce(map, fn {x, y}, map ->
        put_in(map, [y, x], "#")
      end)
    place_grain(map, {500, 0}, 0)
    #render_map(map)
  end

  def place_grain(map, {curr_x, curr_y}, num_grains) do
    down = get_in(map, [curr_y + 1, curr_x])
    diag_left = get_in(map, [curr_y + 1, curr_x - 1])
    diag_right = get_in(map, [curr_y + 1, curr_x + 1])
    cond do
      down == "." -> place_grain(map, {curr_x, curr_y + 1}, num_grains)
      diag_left == "." -> place_grain(map, {curr_x - 1, curr_y + 1}, num_grains)
      diag_right == "." -> place_grain(map, {curr_x + 1, curr_y + 1}, num_grains)
      down == nil -> {map, num_grains}
      map[0][500] == "o" -> {map, num_grains}
      true -> map = put_in(map, [curr_y, curr_x], "o"); place_grain(map, {500, 0}, num_grains + 1)
    end
  end

  def render_map(map) do
    y_vals = Map.keys(map)
      |> Enum.sort
    x_vals = Map.keys(map[y_vals |> Enum.at(0)])
      |> Enum.sort
    y_vals |> Enum.map(fn y ->
      x_vals |> Enum.map(fn x ->
        IO.write(map[y][x])
      end)
      IO.puts("")
    end)
    IO.puts("\n\n")
    map
  end
end

{:ok, input} = File.read('2022/14/input.txt')

DayFourteen.count_sand(input, false) |> elem(1) |> IO.inspect(label: "# grains without floor")
DayFourteen.count_sand(input, true) |> elem(1) |> IO.inspect(label: "# grains with floor")
#  |> IO.inspect(charlist: :as_lists)
#IO.inspect(a[164][368])
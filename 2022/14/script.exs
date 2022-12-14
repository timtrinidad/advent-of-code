# https://adventofcode.com/2022/day/14

defmodule DayFourteen do
  @doc """
    Given an input, keep dropping sand until the source is blocked (is_solid_floor = true)
    or until sand reaches the abyss (is_solid_floor = false)
  """
  def count_sand(input, is_solid_floor) do
    wall_paths = input
    |> String.trim
    |> String.split("\n")
    |> Enum.map(fn x -> x
       # Break each path into numeric tuples (coordinates)
       |> String.split(" -> ")
       |> Enum.map(fn x -> x |> String.split(",") |> Enum.map(&String.to_integer/1) |> List.to_tuple end)
    end)

    # Consider the highest y index to be 1 more than the highest path coordinate
    y_max = wall_paths |> Enum.concat |> Enum.map(&elem(&1, 1)) |> Enum.max |> Kernel.+(1)

    # Initialize a map of empty maps from 0 to y_max
    map = Enum.reduce(0..y_max, %{}, fn y, acc -> put_in(acc, [y], %{}) end)

    # Expand each path into individual coordinates, flatten
    wall_points = wall_paths
      |> Enum.map(fn [first_point | path] ->
        path |> Enum.flat_map_reduce(first_point, fn {point_x, point_y}, {previous_x, previous_y} ->
          # Traverse each vertext outputting individual coordinates
          points = for x <- previous_x..point_x do
            for y <- previous_y..point_y, do: {x, y}
          end |> Enum.concat
          {points, {point_x, point_y}}
        end) |> elem(0)
      end) |> Enum.concat

    # Mark wall points in the map
    map = wall_points
      |> Enum.reduce(map, fn {x, y}, map ->
        put_in(map, [y, x], "#")
      end)

    # Drop the first grain of sand
    place_grain(map, {500, 0}, y_max, is_solid_floor, 0)
  end

  @doc "Recursively drop grain until either a grain reaches the abyss (is_solid_floor = false) or until the source is blocked (is_solid_floor = true)"
  def place_grain(map, {curr_x, curr_y}, floor, is_solid_floor, num_grains) do
    # Determine if each possible next move is empty
    down_empty? = get_in(map, [curr_y + 1, curr_x]) == nil
    diag_left_empty? = get_in(map, [curr_y + 1, curr_x - 1]) == nil
    diag_right_empty? = get_in(map, [curr_y + 1, curr_x + 1]) == nil
    cond do
      # If our current height is higher than the floor (since 0 is highest)
      # Check down, diag_left, and then diag_right. Move grain to first of those available spots.
      curr_y < floor && down_empty? -> place_grain(map, {curr_x, curr_y + 1}, floor, is_solid_floor, num_grains)
      curr_y < floor && diag_left_empty? -> place_grain(map, {curr_x - 1, curr_y + 1}, floor, is_solid_floor, num_grains)
      curr_y < floor && diag_right_empty? -> place_grain(map, {curr_x + 1, curr_y + 1}, floor, is_solid_floor, num_grains)
      # If we've reached the abyss, stop dropping sand (base case)
      !is_solid_floor && curr_y == floor -> {map, num_grains}
      # If the floor is solid and we've reached it, stop moving sand. Start dropping a new one.
      is_solid_floor && curr_y == floor -> map = put_in(map, [floor, curr_x], "o"); place_grain(map, {500, 0}, floor, is_solid_floor, num_grains + 1)
      # If the entrance is blocked, stop dropping sand (base case)
      map[0][500] == "o" -> {map, num_grains}
      # Grain of sand has stopped moving, no available moves. Drop a new grain.
      true -> map = put_in(map, [curr_y, curr_x], "o"); place_grain(map, {500, 0}, floor, is_solid_floor, num_grains + 1)
    end
  end

  @doc "Render a map to the screen"
  def render_map(map) do
    {y_min, y_max} = map |> Map.keys |> Enum.min_max
    {x_min, x_max} = map |> Map.values |> Enum.flat_map(fn row -> row |> Map.keys end) |> Enum.min_max
    for y <- y_min..y_max do
      for x <- x_min-5..x_max+5 do
        IO.write(get_in(map, [y, x]) || ".")
      end
      IO.puts("")
    end
  end
end

{:ok, input} = File.read('2022/14/input.txt')

{map, num_grains} = DayFourteen.count_sand(input, false);
DayFourteen.render_map(map);
num_grains |> IO.inspect(label: "# grains without floor")
{map, num_grains} = DayFourteen.count_sand(input, true)
DayFourteen.render_map(map);
num_grains |> IO.inspect(label: "# grains with floor")

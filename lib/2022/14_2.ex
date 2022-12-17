import AOC

# https://adventofcode.com/2022/day/14
aoc 2022, 14_2 do
  def p1(input) do
    {_, num_grains} = count_sand(input, false)
    num_grains |> IO.inspect(label: "# grains without floor")
  end

  def p2(input) do
    {_, num_grains} = count_sand(input, true)
    num_grains |> IO.inspect(label: "# grains with floor")
  end

  @doc """
    Given an input, keep dropping sand until the source is blocked (is_solid_floor = true)
    or until sand reaches the abyss (is_solid_floor = false)
  """
  def count_sand(input, is_solid_floor) do
    # Generate a list of vertex paths from a list (a,b -> a,c -> a,d) to [[{a,b},{a,c}][{a,c},{a,d}]
    wall_paths =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.flat_map(fn x ->
        [head | tail] =
          x
          |> String.split(" -> ")
          |> Enum.map(fn x ->
            x |> String.split(",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
          end)

        Enum.map_reduce(tail, head, fn x, acc ->
          {[acc, x], x}
        end)
        |> elem(0)
      end)

    # Put all occupied coordinates in a set
    set = MapSet.new()

    set =
      wall_paths
      |> Enum.reduce(set, fn [{start_x, start_y}, {end_x, end_y}], set ->
        Enum.reduce(start_x..end_x, set, fn x, set ->
          Enum.reduce(start_y..end_y, set, fn y, set ->
            MapSet.put(set, {x, y})
          end)
        end)
      end)

    y_max = set |> Enum.map(&elem(&1, 1)) |> Enum.max() |> Kernel.+(1)
    place_grain(set, {500, 0}, y_max, is_solid_floor, 0)
  end

  @doc "Recursively drop grain until either a grain reaches the abyss (is_solid_floor = false) or until the source is blocked (is_solid_floor = true)"
  def place_grain(set, {curr_x, curr_y}, floor, is_solid_floor, num_grains) do
    # Determine if each possible next move is empty
    down_empty? = !MapSet.member?(set, {curr_x, curr_y + 1})
    diag_left_empty? = !MapSet.member?(set, {curr_x - 1, curr_y + 1})
    diag_right_empty? = !MapSet.member?(set, {curr_x + 1, curr_y + 1})

    cond do
      # If our current height is higher than the floor (since 0 is highest)
      # Check down, diag_left, and then diag_right. Move grain to first of those available spots.
      curr_y < floor && down_empty? ->
        place_grain(set, {curr_x, curr_y + 1}, floor, is_solid_floor, num_grains)

      curr_y < floor && diag_left_empty? ->
        place_grain(set, {curr_x - 1, curr_y + 1}, floor, is_solid_floor, num_grains)

      curr_y < floor && diag_right_empty? ->
        place_grain(set, {curr_x + 1, curr_y + 1}, floor, is_solid_floor, num_grains)

      # If we've reached the abyss, stop dropping sand (base case)
      !is_solid_floor && curr_y == floor ->
        {set, num_grains}

      # If the floor is solid and we've reached it, stop moving sand. Start dropping a new one.
      is_solid_floor && curr_y == floor ->
        map = MapSet.put(set, {curr_x, floor})
        place_grain(map, {500, 0}, floor, is_solid_floor, num_grains + 1)

      # If the entrance is blocked, stop dropping sand (base case)
      MapSet.member?(set, {500, 0}) ->
        {set, num_grains}

      # Grain of sand has stopped moving, no available moves. Drop a new grain.
      true ->
        set = MapSet.put(set, {curr_x, curr_y})
        place_grain(set, {500, 0}, floor, is_solid_floor, num_grains + 1)
    end
  end
end

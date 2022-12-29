import AOC

# https://adventofcode.com/2022/day/22
aoc 2022, 22 do
  @compass ["E", "S", "W", "N"]

  def p1(input) do
    {map, directions} = parse_input(input)

    start_x = map |> get_map_start
    wraps = map |> get_2d_wrapping_rules

    follow_directions(map, wraps, directions, {start_x, 1, ""})
    |> IO.inspect()
    |> calculate_password
    |> IO.inspect()
  end

  def p2(input) do
    {map, directions} = parse_input(input)

    start_x = map |> get_map_start
    wraps = map |> get_3d_wrapping_rules

    follow_directions(map, wraps, directions, {start_x, 1, ""})
    |> IO.inspect()
    |> calculate_password
    |> IO.inspect()
  end

  def parse_input(input) do
    [map_input, directions_input] = input |> String.split("\n\n")
    {parse_map(map_input), parse_directions(directions_input)}
  end

  @doc "Parse the top half of the input"
  def parse_map(input) do
    map = input |> String.split("\n")
    max_width = map |> Enum.map(&String.length/1) |> Enum.max()

    map
    |> Enum.map(fn line ->
      String.graphemes(line)
      |> pad_line(max_width)
      |> list_to_map
    end)
    |> list_to_map
  end

  @doc "Parse the bottom half of the input into repeated cardinal directions. E.g. NNNNEEWWWWWEESSSSS"
  def parse_directions(input) do
    {_, directions} =
      Regex.scan(~r/\d+|[RL]/, input)
      |> Enum.reduce({0, []}, fn [x], {compass_idx, acc} ->
        case x do
          "L" ->
            {rem(compass_idx + 3, 4), acc}

          "R" ->
            {rem(compass_idx + 1, 4), acc}

          num ->
            {compass_idx, acc ++ for(_ <- 1..String.to_integer(num), do: compass_idx)}
        end
      end)

    directions
  end

  @doc "Utility to convert a list to a 1-indexed map"
  def list_to_map(list) do
    list = list |> Enum.with_index()
    for {v, k} <- list, into: %{}, do: {k + 1, v}
  end

  @doc "Pad a line to a given length"
  def pad_line(list, length) do
    if length(list) < length,
      do: list ++ for(_ <- length(list)..(length - 1), do: " "),
      else: list
  end

  @doc "Determine where we start on the map - the first non empty cell on the first row"
  def get_map_start(map) do
    map[1]
    |> Enum.filter(fn {_, v} -> v == "." end)
    |> Enum.map(fn {k, _} -> k end)
    |> Enum.min()
  end

  @doc "Create a dictionary of all wrapping rules for 2d (vertical/horizontal)"
  def get_2d_wrapping_rules(map) do
    width = map_size(map[1])
    height = map_size(map)
    rules = %{}

    rules =
      1..height
      |> Enum.reduce(rules, fn y, acc ->
        {min, max} = get_nonempty_bounds(map[y])

        acc
        |> put_in([{min - 1, y, "W"}], {max, y, 0})
        |> put_in([{max + 1, y, "E"}], {min, y, 0})
      end)

    rules =
      1..width
      |> Enum.reduce(rules, fn x, acc ->
        {min, max} = get_nonempty_bounds(get_col(map, x))

        acc
        |> put_in([{x, min - 1, "N"}], {x, max, 0})
        |> put_in([{x, max + 1, "S"}], {x, min, 0})
      end)

    rules
  end

  @doc "Create 3d wrapping rules along with how directions change when wrapped"
  def get_3d_wrapping_rules(map) do
    width = map_size(map[1])
    height = map_size(map)
    edge_length = abs(width - height)

    rules = %{}

    face_starts =
      Enum.reduce(1..width//edge_length, [], fn x, acc ->
        Enum.reduce(1..height//edge_length, acc, fn y, acc ->
          if !is_empty(map, x, y), do: acc ++ [{x, y}], else: acc
        end)
      end)

    corners =
      face_starts
      |> Enum.flat_map(fn {x, y} ->
        [
          {x, y},
          {x + edge_length - 1, y},
          {x, y + edge_length - 1},
          {x + edge_length - 1, y + edge_length - 1}
        ]
      end)
      |> Enum.reduce([], fn {x, y}, acc ->
        acc ++ [{x, y, corner_type(map, x, y)}]
      end)

    #     A  B
    #      _ _
    #   E |_|_|  C
    #   F_|_| D
    # E |_|_| C
    # A |_| G
    #    B
    #
    #

    rules =
      corners
      |> Enum.reduce(rules, fn {x, y, corner}, acc ->
        case corner do
          # F
          {:concave, :nw} ->
            Enum.reduce(1..edge_length, acc, fn dist, acc ->
              acc
              |> put_in([{x - dist, y - 1, "N"}], {x, y - dist, 1})
              |> put_in([{x - 1, y - dist, "W"}], {x - dist, y, 3})
            end)

          # D and G
          {:concave, :se} ->
            Enum.reduce(1..edge_length, acc, fn dist, acc ->
              acc
              |> put_in([{x + dist, y + 1, "S"}], {x, y + dist, 1})
              |> put_in([{x + 1, y + dist, "E"}], {x + dist, y, 3})
            end)

          _ ->
            acc
        end
      end)

    # A
    rules =
      Enum.reduce(1..edge_length, rules, fn dist, acc ->
        acc
        |> put_in([{50 + dist, 1 - 1, "N"}], {1, 150 + dist, 1})
        |> put_in([{1 - 1, 150 + dist, "W"}], {50 + dist, 1, 3})
      end)

    # C
    rules =
      Enum.reduce(1..edge_length, rules, fn dist, acc ->
        acc
        |> put_in([{150 + 1, 51 - dist, "E"}], {100, 100 + dist, 2})
        |> put_in([{100 + 1, 100 + dist, "E"}], {150, 51 - dist, 2})
      end)

    # B
    rules =
      Enum.reduce(1..edge_length, rules, fn dist, acc ->
        acc
        |> put_in([{100 + dist, 1 - 1, "N"}], {0 + dist, 200, 0})
        |> put_in([{0 + dist, 200 + 1, "S"}], {100 + dist, 1, 0})
      end)

    # E
    rules =
      Enum.reduce(1..edge_length, rules, fn dist, acc ->
        acc
        |> put_in([{1 - 1, 100 + dist, "W"}], {51, 51 - dist, 2})
        |> put_in([{51 - 1, 51 - dist, "W"}], {1, 100 + dist, 2})
      end)

    rules
  end

  @doc "Get all cells for a given column as a single Map"
  def get_col(map, x) do
    map |> Enum.map(fn {y, row} -> {y, row[x]} end)
  end

  @doc "Determine the min/max positions for non-empty cells for a given list (row or column)"
  def get_nonempty_bounds(list) do
    list |> Enum.filter(fn {_, v} -> v != " " end) |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
  end

  @doc "Recursively follow directions until no more directions in the queue."
  def follow_directions(
        map,
        wraps,
        [curr_direction | next_directions],
        {x, y, _}
      ) do
    {next_x, next_y, compass_shift} =
      case Enum.at(@compass, curr_direction) do
        "S" -> {x, y + 1}
        "N" -> {x, y - 1}
        "E" -> {x + 1, y}
        "W" -> {x - 1, y}
      end
      |> wrap(wraps, curr_direction)

    {next_x, next_y, compass_shift} =
      case map[next_y][next_x] do
        "." -> {next_x, next_y, compass_shift}
        "#" -> {x, y, 0}
      end

    next_directions =
      if compass_shift > 0,
        do: next_directions |> Enum.map(fn dir -> rem(dir + compass_shift, 4) end),
        else: next_directions

    follow_directions(
      map,
      wraps,
      next_directions,
      {next_x, next_y, curr_direction}
    )
  end

  # Base case
  def follow_directions(_, _, [], coords), do: coords

  @doc "Determine the actual coordinates we should be at if end up in a point that is wrapped"
  def wrap({x, y}, wraps, dir) do
    Map.get(wraps, {x, y, Enum.at(@compass, dir)}, {x, y, 0})
  end

  @doc "Calculate the password from the final coordinates and direction"
  def calculate_password({x, y, dir}) do
    dir =
      case Enum.at(@compass, dir) do
        "E" -> 0
        "S" -> 1
        "W" -> 2
        "N" -> 3
      end

    4 * x + 1000 * y + dir
  end

  @doc "Determine if a given cell is empty"
  def is_empty(map, x, y),
    do: !Map.has_key?(map, y) || !Map.has_key?(map[y], x) || map[y][x] == " "

  @doc "For a given point, determine if we have a convex/concave corner and what direction"
  def corner_type(map, x, y) do
    adjacent_directions_not_empty =
      !is_empty(map, x + 1, y) && !is_empty(map, x - 1, y) && !is_empty(map, x, y + 1) &&
        !is_empty(map, x, y - 1)

    cond do
      adjacent_directions_not_empty && is_empty(map, x + 1, y + 1) && !is_empty(map, x + 1, y - 1) &&
        !is_empty(map, x - 1, y + 1) && !is_empty(map, x - 1, y - 1) ->
        {:concave, :se}

      adjacent_directions_not_empty && !is_empty(map, x + 1, y + 1) && is_empty(map, x + 1, y - 1) &&
        !is_empty(map, x - 1, y + 1) && !is_empty(map, x - 1, y - 1) ->
        {:concave, :ne}

      adjacent_directions_not_empty && !is_empty(map, x + 1, y + 1) &&
        !is_empty(map, x + 1, y - 1) && is_empty(map, x - 1, y + 1) &&
          !is_empty(map, x - 1, y - 1) ->
        {:concave, :sw}

      adjacent_directions_not_empty && !is_empty(map, x + 1, y + 1) &&
        !is_empty(map, x + 1, y - 1) && !is_empty(map, x - 1, y + 1) &&
          is_empty(map, x - 1, y - 1) ->
        {:concave, :nw}

      is_empty(map, x - 1, y) && !is_empty(map, x + 1, y) && is_empty(map, x, y - 1) &&
          !is_empty(map, x, y + 1) ->
        {:convex, :nw}

      is_empty(map, x - 1, y) && !is_empty(map, x + 1, y) && !is_empty(map, x, y - 1) &&
          is_empty(map, x, y + 1) ->
        {:convex, :sw}

      !is_empty(map, x - 1, y) && is_empty(map, x + 1, y) && is_empty(map, x, y - 1) &&
          !is_empty(map, x, y + 1) ->
        {:convex, :ne}

      !is_empty(map, x - 1, y) && is_empty(map, x + 1, y) && !is_empty(map, x, y - 1) &&
          is_empty(map, x, y + 1) ->
        {:convex, :se}

      true ->
        {:none, :none}
    end
  end

  @doc "Render a map"
  def render(map, curr_x, curr_y) do
    height = map_size(map)
    width = map_size(map[1])

    IO.puts("\n\n")

    for y <- 1..height do
      for x <- 1..width do
        if x == curr_x && y == curr_y, do: IO.write("X"), else: IO.write(map[y][x])
      end

      IO.puts("")
    end

    :timer.sleep(100)
  end
end

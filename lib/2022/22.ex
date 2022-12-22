import AOC

aoc 2022, 22 do
  def p1(input) do
    {map, directions} = parse_input(input)

    start_x = map |> get_map_start
    wraps = map |> get_2d_wrapping_rules

    follow_directions(map, wraps, directions, {start_x, 1, ""})
    |> IO.inspect()
    |> calculate_password
    |> IO.inspect()
  end

  def p2(_) do
  end

  def parse_input(input) do
    [map_input, directions_input] = input |> String.split("\n\n")
    {parse_map(map_input), parse_directions(directions_input)}
  end

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

  def parse_directions(input) do
    compass = ["E", "S", "W", "N"]

    {_, directions} =
      Regex.scan(~r/\d+|[RL]/, input)
      |> Enum.reduce({0, []}, fn [x], {compass_idx, acc} ->
        case x do
          "L" ->
            {rem(compass_idx + 3, 4), acc}

          "R" ->
            {rem(compass_idx + 1, 4), acc}

          num ->
            {compass_idx,
             acc ++ for(_ <- 1..String.to_integer(num), do: Enum.at(compass, compass_idx))}
        end
      end)

    directions
  end

  def list_to_map(list) do
    list = list |> Enum.with_index()
    for {v, k} <- list, into: %{}, do: {k + 1, v}
  end

  def pad_line(list, length) do
    if length(list) < length,
      do: list ++ for(_ <- length(list)..(length - 1), do: " "),
      else: list
  end

  def get_map_start(map) do
    map[1]
    |> Enum.filter(fn {_, v} -> v == "." end)
    |> Enum.map(fn {k, _} -> k end)
    |> Enum.min()
  end

  def get_2d_wrapping_rules(map) do
    width = map_size(map[1])
    height = map_size(map)
    %{
      row: for(y <- 1..height, do: {y, get_nonempty_bounds(map[y])}) |> Enum.into(%{}),
      column: for(x <- 1..width, do: {x, get_nonempty_bounds(get_col(map, x))}) |> Enum.into(%{})
    }
  end

  def get_col(map, x) do
    map |> Enum.map(fn {y, row} -> {y, row[x]} end)
  end

  def get_nonempty_bounds(list) do
      list |> Enum.filter(fn {_, v} -> v != " " end) |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
  end

  def follow_directions(
        map,
        wraps,
        [curr_direction | next_directions],
        {x, y, _}
      ) do
    #    IO.inspect({x, y, curr_direction, col_bounds[x], row_bounds[y]})
    {next_x, next_y} =
      case curr_direction do
        "S" -> {x, wrap(y+1, wraps.column[x])}
        "N" -> {x, wrap(y-1, wraps.column[x])}
        "E" -> {wrap(x+1, wraps.row[y]), y}
        "W" -> {wrap(x-1, wraps.row[y]), y}
      end

    {next_x, next_y} = if map[next_y][next_x] == ".", do: {next_x, next_y}, else: {x, y}

    follow_directions(
      map,
      wraps,
      next_directions,
      {next_x, next_y, curr_direction}
    )
  end

  def follow_directions(_, _, [], coords), do: coords

  def wrap(val, {min, max}) do
    cond do
      val > max -> min
      val < min -> max
      true -> val
    end
  end


  def calculate_password({x, y, dir}) do
    dir =
      case dir do
        "E" -> 0
        "S" -> 1
        "W" -> 2
        "N" -> 3
      end

    4 * x + 1000 * y + dir
  end
end

import AOC

# https://adventofcode.com/2021/day/22
aoc 2021, 22 do
  def p1(input) do
    # Unoptimized way of truncating ranges within the -50 to 50 area
    parse_input(input)
    |> Enum.reduce(MapSet.new(), fn {on, ranges}, acc ->
      # Truncate ranges. Force incrementing (//1) to ensure ranges like 5000..50 don't yield any results
      [range_x, range_y, range_z] =
        ranges |> Enum.map(fn min..max -> max(min, -50)..min(max, 50)//1 end)

      # Expand into a tuple
      vals = for x <- range_x, y <- range_y, z <- range_z, do: {x, y, z}

      # Add or remove coordinates from the set
      if on,
        do: MapSet.union(acc, MapSet.new(vals)),
        else: MapSet.difference(acc, MapSet.new(vals))
    end)
    |> MapSet.size()
  end

  def p2(input) do
    parse_input(input)
  end

  @doc "Parse input into {on=true|false, [range_x, range_y, rangz_z]}"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [status | ranges] =
        Regex.run(~r/^([a-z]+) x=(.+?)\.\.(.+?),y=(.+?)\.\.(.+?),z=(.+?)\.\.(.+?)$/, line,
          capture: :all_but_first
        )

      [x_min, x_max, y_min, y_max, z_min, z_max] = ranges |> Enum.map(&String.to_integer/1)
      {status == "on", [x_min..x_max, y_min..y_max, z_min..z_max]}
    end)
  end
end

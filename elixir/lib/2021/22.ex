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
      if on == 1,
        do: MapSet.union(acc, MapSet.new(vals)),
        else: MapSet.difference(acc, MapSet.new(vals))
    end)
    |> MapSet.size()
  end

  def p2(input) do
    # Implemented from https://www.reddit.com/r/adventofcode/comments/rlxhmg/comment/hpizza8/?utm_source=reddit&utm_medium=web2x&context=3
    # Was going to keep track of various intersections with set theory but ran out of time. May come back to do so in the future.
    counter =
      parse_input(input)
      |> Enum.reduce(%{}, fn {status, [x1..x2, y1..y2, z1..z2]}, acc ->
        acc =
          acc
          |> Enum.reduce(acc, fn {{ex1, ex2, ey1, ey2, ez1, ez2}, val}, acc ->
            # Find intersections
            ix1 = max(x1, ex1)
            ix2 = min(x2, ex2)
            iy1 = max(y1, ey1)
            iy2 = min(y2, ey2)
            iz1 = max(z1, ez1)
            iz2 = min(z2, ez2)

            if ix1 <= ix2 && iy1 <= iy2 && iz1 <= iz2,
              do: Map.update(acc, {ix1, ix2, iy1, iy2, iz1, iz2}, -val, &(&1 - val)),
              else: acc
          end)

        if status == 1, do: Map.update(acc, {x1, x2, y1, y2, z1, z2}, 1, &(&1 + 1)), else: acc
      end)

    counter
    |> Enum.reduce(0, fn {{x1, x2, y1, y2, z1, z2}, val}, acc ->
      acc + (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1) * val
    end)
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
      status = if status == "on", do: 1, else: -1
      {status, [x_min..x_max, y_min..y_max, z_min..z_max]}
    end)
  end
end

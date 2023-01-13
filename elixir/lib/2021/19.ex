import AOC

# https://adventofcode.com/2021/day/19
aoc 2021, 19 do
  def p1(input) do
    beacon_sets = parse_input(input)
    reduce(beacon_sets) |> Enum.filter(fn {_, _, _, scanner?} -> !scanner? end) |> length
  end

  def p2(input) do
    beacon_sets = parse_input(input)
    scanners = reduce(beacon_sets) |> Enum.filter(fn {_, _, _, scanner?} -> scanner? end)
    num_scanners = length(scanners)
    pairs = for i <- 0..(num_scanners - 1), j <- 0..(num_scanners - 1), i != j, do: {i, j}

    # For all pairs of scanners, calculate manhattan distance and find max
    pairs
    |> Enum.map(fn {i, j} ->
      {x1, y1, z1, _} = Enum.at(scanners, i)
      {x2, y2, z2, _} = Enum.at(scanners, j)
      abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)
    end)
    |> Enum.max()
  end

  @doc "Parse the input into a list of list of beacons, e.g. [[{x,y,z},{x,y,z}],[{x,y,z}]]"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n\n")
    |> Enum.map(fn chunk ->
      [_ | beacons] = chunk |> String.split("\n")

      beacons
      |> Enum.map(fn beacon ->
        [x, y, z] = beacon |> String.split(",") |> Enum.map(&String.to_integer/1)
        {x, y, z, false}
      end)
      |> MapSet.new()
      |> MapSet.put({0, 0, 0, true})
    end)
  end

  @doc "Recursively combine multiple sets until there's one set left"
  def reduce([set | []]), do: set

  def reduce(sets) do
    num_sets = length(sets)

    # Compare each set against all other sets to find a pair with at least 12 common points
    pairs = for i <- 0..(num_sets - 1), j <- 0..(num_sets - 1), i != j, do: {i, j}

    {i, j} =
      pairs
      |> Enum.find(fn {i, j} ->
        a = Enum.at(sets, i)
        b = Enum.at(sets, j)
        orig_size = MapSet.size(a) + MapSet.size(b)
        combined = union(a, b)
        common_size = if combined == nil, do: 0, else: orig_size - MapSet.size(combined)
        common_size >= 12
      end)

    # Replace the original two sets with the combined set
    a = Enum.at(sets, i)
    b = Enum.at(sets, j)
    sets = sets |> List.delete_at(max(i, j)) |> List.delete_at(min(i, j))
    reduce([union(a, b) | sets])
  end

  @doc "Given two sets of beacons, determine any transformations/translations which need to be done and combine into a single set"
  def union(a, b) do
    # Generate fingerprints for each set of beacons
    fpa = generate_fingerprints(a)
    fpb = generate_fingerprints(b)

    # Determine which fingerprints are common between the two sets
    intersection = MapSet.intersection(MapSet.new(Map.keys(fpa)), MapSet.new(Map.keys(fpb)))

    # If no fingerprints are shared, there are no common points. Move on.
    if MapSet.size(intersection) == 0 do
      nil
    else
      # Arbitrarily go through the first intersection - two points in set a
      # which correspond to two points in set b.
      int1 = intersection |> Enum.at(0)
      # Get the two points in each set which correspond to this intersection
      {pa1, pa2} = fpa[int1]
      {pb1, pb2} = fpb[int1]

      # Since two points can match mirrored states, try to combine both directinos (pb1,pb2 and pb2,pb1) to see
      # which one returns the largest number of common points.
      set1 = transform_and_combine(a, b, pa1, pa2, pb1, pb2)
      set2 = transform_and_combine(a, b, pa1, pa2, pb2, pb1)

      if MapSet.size(set1) < MapSet.size(set2), do: set1, else: set2
    end
  end

  @doc "Given two sets and two pairs of points, determine how to transform the second set to combine the two sets"
  def transform_and_combine(seta, setb, pa1, pa2, pb1, pb2) do
    # Determine the deltas for xyz between the two points in set a.
    {pa1x, pa1y, pa1z, _} = pa1
    {pa2x, pa2y, pa2z, _} = pa2
    dx = pa2x - pa1x
    dy = pa2y - pa1y
    dz = pa2z - pa1z

    # Find the transformation which matches the two points from set a with the two points from set b
    transformation =
      transformations()
      |> Enum.find(fn transformation ->
        {p1x, p1y, p1z, _} = transform(pb1, transformation)
        {p2x, p2y, p2z, _} = transform(pb2, transformation)
        p2x - dx == p1x && p2y - dy == p1y && p2z - dz == p1z
      end)

    # Transform the reference point and get the delta for each dimension
    {refx, refy, refz, _} = transform(pb1, transformation)
    dx = pa1x - refx
    dy = pa1y - refy
    dz = pa1z - refz

    # Tranform and shift each point in the second set before combining them into a single set
    setb =
      setb
      |> Enum.map(fn point ->
        {x, y, z, scanner?} = transform(point, transformation)
        {x + dx, y + dy, z + dz, scanner?}
      end)
      |> MapSet.new()

    MapSet.union(seta, setb)
  end

  @doc "Generate a set of fingerprints for a set of beacons against all other beacons in the same set. "
  def generate_fingerprints(beacons) do
    # Hint from https://www.reddit.com/r/adventofcode/comments/rjpf7f/comment/hp551kv/?utm_source=share&utm_medium=web2x&context=3
    # Double for loop to process all beacons against all other beacons
    beacons
    |> Enum.reduce(%{}, fn b1 = {x1, y1, z1, _}, acc ->
      beacons
      |> Enum.reduce(acc, fn b2 = {x2, y2, z2, _}, acc ->
        if b1 == b2 do
          acc
        else
          dx = abs(x1 - x2)
          dy = abs(y1 - y2)
          dz = abs(z1 - z2)
          # Technically the distance is the sqrt of this value, but it doesn't matter
          # in this case
          distance = dx * dx + dy * dy + dz * dz
          min_diff = Enum.min([dx, dy, dz])
          max_diff = Enum.max([dx, dy, dz])
          # Store the fingerprint and which two points were used for that fingerprint
          Map.put(acc, {distance, min_diff, max_diff}, {b1, b2})
        end
      end)
    end)
  end

  @doc "Enumerate all possible combinations of reordering and flipping the axes"
  def transformations() do
    [
      {1, 2, 3},
      {1, 3, 2},
      {2, 1, 3},
      {2, 3, 1},
      {3, 1, 2},
      {3, 2, 1},
      {-1, 2, 3},
      {-1, 3, 2},
      {-2, 1, 3},
      {-2, 3, 1},
      {-3, 1, 2},
      {-3, 2, 1},
      {1, -2, 3},
      {1, -3, 2},
      {2, -1, 3},
      {2, -3, 1},
      {3, -1, 2},
      {3, -2, 1},
      {1, 2, -3},
      {1, 3, -2},
      {2, 1, -3},
      {2, 3, -1},
      {3, 1, -2},
      {3, 2, -1},
      {-1, -2, 3},
      {-1, -3, 2},
      {-2, -1, 3},
      {-2, -3, 1},
      {-3, -1, 2},
      {-3, -2, 1},
      {1, -2, -3},
      {1, -3, -2},
      {2, -1, -3},
      {2, -3, -1},
      {3, -1, -2},
      {3, -2, -1},
      {-1, 2, -3},
      {-1, 3, -2},
      {-2, 1, -3},
      {-2, 3, -1},
      {-3, 1, -2},
      {-3, 2, -1},
      {-1, -2, -3},
      {-1, -3, -2},
      {-2, -1, -3},
      {-2, -3, -1},
      {-3, -1, -2},
      {-3, -2, -1}
    ]
  end

  @doc "Given a specific transformation from transformations(), transform the given point"
  def transform({x, y, z, scanner?}, {tx, ty, tz}) do
    point = [x, y, z]

    {
      # Reoder, and flip
      Enum.at(point, abs(tx) - 1) * if(tx < 0, do: -1, else: 1),
      Enum.at(point, abs(ty) - 1) * if(ty < 0, do: -1, else: 1),
      Enum.at(point, abs(tz) - 1) * if(tz < 0, do: -1, else: 1),
      scanner?
    }
  end
end

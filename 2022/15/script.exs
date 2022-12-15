# https://adventofcode.com/2022/day/13

defmodule DayFifteen do
  def exec(input) do
    sensors_and_beacons = parse_input(input)

    # Separate into a list of beacons and sensors
    sensors = Enum.map(sensors_and_beacons, fn {sensor_x, sensor_y, _, _, distance} -> {sensor_x, sensor_y, distance} end)
    beacons = MapSet.new(sensors_and_beacons |> Enum.map(fn {_, _, beacon_x, beacon_y, _} -> {beacon_x, beacon_y} end))

    # For each possible y, determine a list of ranges where a beacon cannot exist
    row_empty_ranges = for y <- 0..4000000 do
      # For each sensor, given a y, we know the min x and max x between which there cannot be a beacon
      empty_x_ranges = Enum.reduce(sensors, [], fn {sensor_x, sensor_y, max_distance}, acc ->
        # Current y distance to sensor
        y_dist = abs(sensor_y - y)
        # To be within range of sensor, the x needs to be within the remaining available distance
        x_dist_max = max_distance - y_dist
        empty_range_min = sensor_x-x_dist_max
        empty_range_max = sensor_x+x_dist_max
        # Do nothing if the y distance is already further than the max distance to the sensor.
        # Otherwise, store the range of empty x values for this given sensor for this given row
        if x_dist_max > 0, do: acc ++ [empty_range_min..empty_range_max], else: acc
      end) |> ranges_union # Union ranges to remove overlap
      {y, empty_x_ranges}
    end

    # Part 1
    # Get the ranges for only for a single row
    part_1_y = 2000000
    {_, [part_1_range]} = row_empty_ranges |> Enum.find(fn {y, _} -> y == part_1_y end)
    beacons
      |> Enum.reduce(MapSet.new(part_1_range), fn {beacon_x, beacon_y}, acc ->
        # Remove from the range places where there's a known beacon
        if beacon_y == part_1_y, do: MapSet.delete(acc, beacon_x), else: acc
      end)
      |> MapSet.size
      |> IO.inspect(label: "# possible spots at y=#{part_1_y}")

    # Part 2
    # We only know that one single row has a single point that is not empty
    # And in that row, there's only a single point that is empty.
    # Find the row that has 2 ranges (if all is empty there would only be one range)
    {y, empty_xs} = row_empty_ranges
      |> Enum.find(fn {_, empty_ranges} -> length(empty_ranges) > 1 end)
    # We know there's only one point, so get the end of the first range, get the next point over to find the non-empty point
    x = Enum.at(empty_xs, 0).last + 1
    x * 4000000 + y |> IO.inspect(label: "Tuning frequency")
  end

  @doc "Parse the input into beacons and sensors"
  def parse_input(input) do
    input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(fn line ->
        [sensor_x, sensor_y, beacon_x, beacon_y] = Regex.run(~r/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/, line)
          |> tl
          |> Enum.map(&String.to_integer/1)
        distance = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y)
        {sensor_x, sensor_y, beacon_x, beacon_y, distance}
      end)
  end

  @doc "Given a list of ranges, union them into a smaller list of ranges with no overlap"
  # Adoptex from https://stackoverflow.com/a/15273749/1951514
  def ranges_union(ranges) do
    ranges |> Enum.sort |> Enum.reduce([], fn curr_range, acc ->
      case List.last(acc) do
        nil -> [curr_range]
        last_range when last_range.last >= curr_range.first - 1 ->
          List.replace_at(acc, -1, last_range.first..max(last_range.last, curr_range.last))
        _ -> acc ++ [curr_range]
      end
    end)
  end
end

{:ok, input} = File.read('2022/15/input.txt')

DayFifteen.exec(input)
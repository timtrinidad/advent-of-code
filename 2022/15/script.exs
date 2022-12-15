# https://adventofcode.com/2022/day/13

defmodule DayFifteen do
  def exec(input) do
    sensors_and_beacons = input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(fn line ->
        [sensor_x, sensor_y, beacon_x, beacon_y] = Regex.run(~r/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/, line)
          |> tl |> Enum.map(&String.to_integer/1)
        distance = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y)
        {sensor_x, sensor_y, beacon_x, beacon_y, distance}
      end)

    beacons = MapSet.new(sensors_and_beacons |> Enum.map(fn {_, _, beacon_x, beacon_y, _} -> {beacon_x, beacon_y} end))
    x_values = sensors_and_beacons |> Enum.flat_map(fn {sensor_x, _, beacon_x, _, _} -> [sensor_x, beacon_x] end)
    distance_max = sensors_and_beacons |> Enum.map(fn {_, _, _, _, distance} -> distance end) |> Enum.max
    x_min = x_values |> Enum.min |> Kernel.-(distance_max)
    x_max = x_values |> Enum.max |> Kernel.+(distance_max)

    y = 2000000
    Enum.reduce(x_min..x_max, 0, fn x, acc ->
      inside_sensor_range? = Enum.reduce(sensors_and_beacons, false, fn {sensor_x, sensor_y, _, _, max_distance}, acc ->
        point_distance =  abs(sensor_x - x) + abs(sensor_y - y)
        acc || (point_distance <= max_distance)
      end)
      if inside_sensor_range? && !MapSet.member?(beacons, {x, y}), do: acc + 1, else: acc
    end)
  end
end

{:ok, input} = File.read('2022/15/input.txt')

DayFifteen.exec(input)
  |> IO.inspect(charlist: :as_lists)
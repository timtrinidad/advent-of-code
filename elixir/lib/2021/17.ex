import AOC

# https://adventofcode.com/2021/day/17
aoc 2021, 17 do
  def p1(input) do
    target_area = parse_input(input)

    # Try every velocity from {0, 0} to {1000, 1000} to see the max height achieved
    for(vx <- 0..1000, vy <- 0..1000, do: {vx, vy})
    |> Enum.reduce({{0, 0}, 0}, fn vel, {old_vel, curr_max} ->
      res = emulate(target_area, {0, 0}, vel)
      if res != nil && res > curr_max, do: {vel, res}, else: {old_vel, curr_max}
    end)
  end

  def p2(input) do
    target_area = parse_input(input)

    # Try every velocity from {0, 0} to {1000, 1000} to see the max height achieved
    for(vx <- 0..1000, vy <- -1000..1000, do: {vx, vy})
    |> Enum.reduce(0, fn vel, acc ->
      res = emulate(target_area, {0, 0}, vel)
      if res != nil, do: acc + 1, else: acc
    end)
  end

  @doc """
  For a given target and start, determine if the given velocity lands in the target area.
  If it does, returns the max height achieved, otherwise, returns nil
  """
  def emulate(target = {target_x, target_y}, {x, y}, {vx, vy}) do
    # The next point is determined by adding the current velocity
    next = {x + vx, y + vy}

    # Next velocity - x converges to 0, 1 step at a time
    vx =
      cond do
        vx > 0 -> vx - 1
        vx < 0 -> vx + 1
        true -> vx
      end

    # y decreases by 1
    next_vel = {vx, vy - 1}

    _..max_x = target_x
    min_y.._ = target_y

    cond do
      # Point is in target
      x in target_x && y in target_y ->
        y

      # Point has passed the target
      x > max_x || y < min_y ->
        nil

      # Recurse and find max height
      true ->
        max_y = emulate(target, next, next_vel)
        if max_y == nil, do: nil, else: max(y, max_y)
    end
  end

  @doc "Parse the range from the input text"
  def parse_input(input) do
    [x1, x2, y1, y2] =
      Regex.run(~r/target area: x=(.+?)\.\.(.+?), y=(.+?)\.\.(.+)/, input, capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)

    {x1..x2, y1..y2}
  end
end

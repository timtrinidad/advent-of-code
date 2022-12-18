import AOC

aoc 2022, 17 do
  def p1(input) do
    simulate(input, 2022)
  end

  def p2(_) do
  end

  @doc "Kick off a simulation of rocks falling for num_moves"
  def simulate(input, num_moves) do
    moves = input |> String.trim() |> String.graphemes()
    IO.inspect(moves |> length, label: "Move list length")
    locations = make_move(MapSet.new(), generate_rock(0, 3), [0, 1, 2, 3, 4], moves, num_moves)
    #    render(locations)
    locations |> locations_max_y |> Kernel.+(1) |> IO.inspect(label: "Max Y")
  end

  @doc "Recursive function to simulate rock movement."
  def make_move(locations, curr_rock, next_rock_types, [curr_move | next_moves], num_rocks) do
    # Progress
    if rem(num_rocks, 200) == 0, do: IO.puts("# rocks remaining: #{num_rocks}")

    # Base case - no more rocks to drop
    if num_rocks == 0 do
      locations
    else
      # Move the rock to the side if possible - coordinates don't change if it's an impossible move
      {_, curr_rock} = move_rock(locations, curr_rock, curr_move)
      # Move rock down if possible
      {valid_y_move, curr_rock} = move_rock(locations, curr_rock, "v")

      # If could not move down, lock current rock coordinates into locations set
      locations =
        if valid_y_move == :ok,
          do: locations,
          else: MapSet.union(locations, MapSet.new(curr_rock))

      locations_max_y = locations_max_y(locations)

      # Move current rock type (head) to the end of the list
      next_rock_types =
        if valid_y_move == :ok,
          do: next_rock_types,
          else: tl(next_rock_types) ++ [hd(next_rock_types)]

      # Decrement the num_rocks counter if we've moved onto a new rock
      num_rocks = if valid_y_move == :ok, do: num_rocks, else: num_rocks - 1

      # If moving onto a new rock, generate a new rock in the span position
      curr_rock =
        if valid_y_move == :ok,
          do: curr_rock,
          else: generate_rock(hd(next_rock_types), locations_max_y + 4)

      # Recursively move rock
      make_move(locations, curr_rock, next_rock_types, next_moves ++ [curr_move], num_rocks)
    end
  end

  @doc "Generate a rock with the bottom at the given base_y"
  def generate_rock(rock_type, base_y) do
    case rock_type do
      0 -> [{2, base_y}, {3, base_y}, {4, base_y}, {5, base_y}]
      1 -> [{3, base_y}, {2, base_y + 1}, {3, base_y + 1}, {4, base_y + 1}, {3, base_y + 2}]
      2 -> [{2, base_y}, {3, base_y}, {4, base_y}, {4, base_y + 1}, {4, base_y + 2}]
      3 -> [{2, base_y}, {2, base_y + 1}, {2, base_y + 2}, {2, base_y + 3}]
      4 -> [{2, base_y}, {3, base_y}, {2, base_y + 1}, {3, base_y + 1}]
    end
  end

  @doc "Move a rock in the specified direction if possible, If not possible, don't change coordinates and return :invalid"
  def move_rock(locations, rock, direction) do
    potential_pos =
      rock
      |> Enum.map(fn {x, y} ->
        case direction do
          ">" -> {x + 1, y}
          "<" -> {x - 1, y}
          "v" -> {x, y - 1}
        end
      end)

    valid_pos = validate_position(locations, potential_pos)
    {valid_pos, if(valid_pos == :ok, do: potential_pos, else: rock)}
  end

  @doc "Validate the new rock position"
  def validate_position(locations, rock_position) do
    {rock_min_x, _} = rock_position |> Enum.min_by(&elem(&1, 0))
    {rock_max_x, _} = rock_position |> Enum.max_by(&elem(&1, 0))
    {_, rock_min_y} = rock_position |> Enum.min_by(&elem(&1, 1))

    cond do
      # Too far left
      rock_min_x < 0 -> :invalid
      # Too far right
      rock_max_x > 6 -> :invalid
      # Too far down
      rock_min_y < 0 -> :invalid
      # Overlaps with existing rock positions
      MapSet.size(MapSet.intersection(locations, MapSet.new(rock_position))) > 0 -> :invalid
      true -> :ok
    end
  end

  @doc "Get the current top of the rock formation"
  def locations_max_y(locations) do
    cond do
      MapSet.size(locations) == 0 -> -1
      true -> locations |> MapSet.to_list() |> Enum.max_by(&elem(&1, 1)) |> elem(1)
    end
  end

  def render(locations) do
    max_y = locations_max_y(locations)
    IO.puts("")

    for y <- max_y..0 do
      for x <- 0..6 do
        IO.write(if MapSet.member?(locations, {x, y}), do: "#", else: ".")
      end

      IO.puts("")
    end
  end
end

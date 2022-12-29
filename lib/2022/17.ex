import AOC

# https://adventofcode.com/2022/day/17
aoc 2022, 17 do
  def p1(input) do
    simulate(input, 2022)
  end

  def p2(input) do
    simulate(input, 1_000_000_000_000)
  end

  @doc "Kick off a simulation of rocks falling for num_moves"
  def simulate(input, num_rocks) do
    moves = input |> String.trim() |> String.graphemes() |> Enum.with_index()
    IO.inspect(moves |> length, label: "Move list length")

    :ets.new(:moves, [:set, :named_table])

    locations = make_move(MapSet.new(), generate_rock(0, 3), [0, 1, 2, 3, 4], moves, num_rocks, 0)
    locations |> locations_max_y |> Kernel.+(1) |> IO.inspect(label: "Max Y")
  end

  @doc "Recursive function to simulate rock movement."
  def make_move(
        locations,
        curr_rock,
        next_rock_types,
        [curr_move | next_moves],
        num_rocks,
        rock_num
      ) do
    # Move the rock to the side if possible - coordinates don't change if it's an impossible move
    {_, curr_rock} = move_rock(locations, curr_rock, elem(curr_move, 0))
    # Move rock down if possible
    {valid_y_move, curr_rock} = move_rock(locations, curr_rock, "v")

    cond do
      # Base case
      num_rocks == 0 ->
        locations

      # Valid move - put current move at end of list and move again
      valid_y_move == :ok ->
        make_move(
          locations,
          curr_rock,
          next_rock_types,
          next_moves ++ [curr_move],
          num_rocks,
          rock_num
        )

      # Put current rock in permanent location
      true ->
        locations = curr_rock |> Enum.reduce(locations, &MapSet.put(&2, &1))
        max_y = locations_max_y(locations)

        # Keep track of if we've seen the current state before - location of the top 20 rows relative to the top
        # and the index of the current vent direction
        vent_index = elem(curr_move, 1)

        cache_key =
          {vent_index,
           MapSet.filter(locations, fn {_, y} -> y > max_y - 20 end)
           |> Enum.map(fn {x, y} -> {x, max_y - y} end)
           |> Enum.sort()}

        {num_rocks, rock_num, locations, max_y} =
          if length(cache = :ets.lookup(:moves, cache_key)) > 0 do
            # We have seen this state before.
            [{_, seen_rock_num, seen_max_y}] = cache

            # Determine how many rocks and rows were added since the last time we saw this state
            delta_max_y = max_y - seen_max_y
            delta_rock_num = rock_num - seen_rock_num
            # How many times can we repeat this delta based on how many rocks we have left?
            repeats = div(num_rocks - seen_rock_num, delta_rock_num)

            # Increment the number of rocks seen, the number of rocks left, the location of all settled rocks,
            # and the max_y based on the pattern
            num_rocks = num_rocks - delta_rock_num * repeats
            rock_num = rock_num + delta_rock_num * repeats

            locations =
              locations
              |> Enum.map(fn {x, y} -> {x, y + delta_max_y * repeats} end)
              |> MapSet.new()

            {num_rocks, rock_num, locations, max_y + delta_max_y * repeats}
          else
            # Otherwise, record that we've seen this state but don't modify any variables
            :ets.insert(:moves, {cache_key, rock_num, max_y})
            {num_rocks, rock_num, locations, max_y}
          end

        # Move curr rock type to the end
        [curr_type | next_types] = next_rock_types
        next_rock_types = next_types ++ [curr_type]

        make_move(
          locations,
          # Make new rock based on next type 4 spots above current max
          generate_rock(hd(next_rock_types), max_y + 4),
          # Move next rock type to end of list
          next_rock_types,
          # Move current move to end of list
          next_moves ++ [curr_move],
          # decrement remaining rock count
          num_rocks - 1,
          rock_num + 1
        )
    end
  end

  @doc "Generate a rock with the bottom at the given base_y"
  def generate_rock(rock_type, base_y) do
    case rock_type do
      # 一
      0 -> [{2, base_y}, {3, base_y}, {4, base_y}, {5, base_y}]
      # 十
      1 -> [{3, base_y}, {2, base_y + 1}, {3, base_y + 1}, {4, base_y + 1}, {3, base_y + 2}]
      # 𠃎
      2 -> [{2, base_y}, {3, base_y}, {4, base_y}, {4, base_y + 1}, {4, base_y + 2}]
      # 丨
      3 -> [{2, base_y}, {2, base_y + 1}, {2, base_y + 2}, {2, base_y + 3}]
      # 田
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

    # If valid pos, return OK with potential position. Otherwise return old position with an error.
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

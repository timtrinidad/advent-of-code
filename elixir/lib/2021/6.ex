import AOC

aoc 2021, 6 do
  def p1(input) do
    fishes = parse_input(input)
    # For each day, process each fish
    1..80
    |> Enum.reduce(fishes, fn _, fishes ->
      {fishes, num_new_fish} =
        fishes
        |> Enum.map_reduce(0, fn fish, acc ->
          # For every fish we wrap from 0 to 6, increment the number of new fish we should add
          if fish == 0, do: {6, acc + 1}, else: {fish - 1, acc}
        end)

      # Append to the list the number of new fish to add. 1..1 adds one fish, 1..2 adds 2, etc.
      # If we want no fish, 1..0 still generates two items (1 and 0). Force only positive
      # increments with "//1"
      fishes ++ for _ <- 1..num_new_fish//1, do: 8
    end)
    |> length
  end

  def p2(input) do
  end

  def parse_input(input) do
    input |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
  end
end

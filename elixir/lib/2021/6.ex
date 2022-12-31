import AOC

aoc 2021, 6 do
  def p1(input) do
    parse_input(input) |> simulate_spawning(80)
  end

  def p2(input) do
    parse_input(input) |> simulate_spawning(256)
  end

  @doc "Simulate the spawning for the given fish for any number of days"
  def simulate_spawning(fishes, num_days) do
    # For each day, process each fish
    1..num_days
    # Reduce the list of fish into a map of days left to number of fish
    |> Enum.reduce(Enum.frequencies(fishes), fn _, fishes ->
      fishes
      # For each "day left", reduce the day. E.g. for 7 fish with 5 days, left, go from %{5 => 7} to %{4 => 7}
      |> Enum.reduce(%{}, fn
        # Account for possible key collisions - i.e. both 7 and 0 will resolve to 6
        {0, num_fish}, acc ->
          Map.update(acc, 6, num_fish, &(&1 + num_fish)) |> put_in([8], num_fish)

        {days_left, num_fish}, acc ->
          Map.update(acc, days_left - 1, num_fish, &(&1 + num_fish))
      end)
    end)
    |> Map.values()
    |> Enum.sum()
  end

  @doc "Split input into a list of integers"
  def parse_input(input) do
    input |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
  end
end

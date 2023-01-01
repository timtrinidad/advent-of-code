# https://adventofcode.com/2021/day/11
import AOC

aoc 2021, 11 do
  def p1(input) do
    parse_input(input) |> do_round(100, 0)
  end

  def p2(input) do
    parse_input(input) |> do_round(-1, 0)
  end

  @doc "Parse input into a map of integers keyed by {x, y} coordinates"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {val, x}, acc ->
        Map.put(acc, {x, y}, String.to_integer(val))
      end)
    end)
  end

  # Base case - no more rounds. Return total number of bursts. If negatuve rounds given, returns round number of 1st sync'd bursting
  def do_round(_, 0, total_bursts), do: total_bursts

  @doc "Process a round - increment all values by one and continuously process bursts until there are no more bursts"
  def do_round(oct, rounds_remaining, total_bursts) do
    # Increment all by 1
    oct =
      oct
      |> Map.keys()
      |> Enum.reduce(oct, fn {x, y}, oct ->
        Map.update!(oct, {x, y}, &(&1 + 1))
      end)

    {oct, num_bursts} = process_bursts(oct, MapSet.new())

    # If negative (unlimited) rounds and num_bursts is 100, all burst at the same time. Return round number.
    if rounds_remaining < 0 && num_bursts == 100 do
      -rounds_remaining
    else
      # Recurse until no more rounds
      do_round(oct, rounds_remaining - 1, total_bursts + num_bursts)
    end
  end

  @doc "Recursively process bursts until there are no more, at which point return map of current values and number of bursts processed"
  def process_bursts(oct, already_burst) do
    # Determine which need to burst - higher than 9 and not already processed
    to_burst =
      oct |> Map.filter(fn {key, val} -> val > 9 && !MapSet.member?(already_burst, key) end)

    if map_size(to_burst) == 0 do
      # No more bursts to process - reset all values above 9 to 0
      oct =
        Map.keys(oct)
        |> Enum.reduce(oct, fn coords, oct ->
          Map.update!(oct, coords, fn val -> if val > 9, do: 0, else: val end)
        end)

      # Return current values and number of bursts
      {oct, MapSet.size(already_burst)}
    else
      # Append all we're going to burst to the "seen" set
      already_burst = to_burst |> Map.keys() |> Enum.reduce(already_burst, &MapSet.put(&2, &1))

      # For each to_burst, increment each neighbor
      oct =
        to_burst
        |> Enum.reduce(oct, fn {{x, y}, _}, oct ->
          [{-1, -1}, {0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}]
          |> Enum.reduce(oct, fn {dx, dy}, oct ->
            neighbor = {x + dx, y + dy}
            if Map.has_key?(oct, neighbor), do: Map.update!(oct, neighbor, &(&1 + 1)), else: oct
          end)
        end)

      # Recurse until no more to burst
      process_bursts(oct, already_burst)
    end
  end
end

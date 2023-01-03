import AOC

# https://adventofcode.com/2021/day/14
aoc 2021, 14 do
  def p1(input) do
    parse_input(input) |> simulate(10)
  end

  def p2(input) do
    parse_input(input) |> simulate(40)
  end

  @doc "Parse the input into a list of characters (template) and a map of tuple pairs to characters (mapping)"
  def parse_input(input) do
    [template, mapping] = input |> String.trim() |> String.split("\n\n")

    mapping =
      mapping
      |> String.split("\n")
      |> Enum.reduce(%{}, fn line, acc ->
        [left, right] = String.split(line, " -> ")
        left = String.graphemes(left) |> List.to_tuple()
        Map.put(acc, left, right)
      end)

    {String.graphemes(template), mapping}
  end

  @doc "Expand for the given number of rounds and calc the diff b/w the highest and lowest frequencies"
  def simulate({template, mapping}, num_rounds) do
    # Create a list of pairs from the template. For example from ["A", "B", "C"] create [{"A", "B"}, {"B", "C"}]
    pair_counts =
      Enum.zip(template, tl(template))
      # Map frequencies, e.g. %{{"A", "B"} => 1, {"B", "C"} => 1}
      |> Enum.frequencies()

    # Run the expansion of each pair for the given number of rounds
    pair_frequencies = expand(pair_counts, mapping, num_rounds)

    frequencies_sorted =
      pair_frequencies
      # Count the frequency of the first letter in each pair (e.g. {B,C} => 2 and {B,D} => 4 becomes B => 6)
      |> Enum.reduce(%{}, fn {{l, _}, val}, acc -> Map.update(acc, l, val, &(&1 + val)) end)
      # Make sure to increment the last letter in the template since it won't appear on the left side of the pair
      |> Map.update!(Enum.at(template, -1), &(&1 + 1))
      |> Map.values()
      |> Enum.sort()

    # Most frequent minus least frequent
    Enum.at(frequencies_sorted, -1) - Enum.at(frequencies_sorted, 0)
  end

  # Base case - no more rounds. Return pair counts
  def expand(pair_counts, _, 0), do: pair_counts

  @doc "Expand the pair counts based on the mapping riules for the given number of rounds"
  def expand(pair_counts, mapping, num_rounds) do
    # Implemented with a hint from https://www.reddit.com/r/adventofcode/comments/rhroca/comment/hosbdtu/?utm_source=share&utm_medium=web2x&context=3
    pair_counts =
      pair_counts
      |> Enum.reduce(%{}, fn {pair, count}, acc ->
        # For each pair we currently have, split it into two separate pairs with the middle char determined by the mapping
        {left, right} = pair
        mid = mapping[pair]

        # Increment the two new pairs by the number of times the original pair exists
        acc
        |> Map.update({left, mid}, count, &(&1 + count))
        |> Map.update({mid, right}, count, &(&1 + count))
      end)

    # Recurse with one less round
    expand(pair_counts, mapping, num_rounds - 1)
  end
end

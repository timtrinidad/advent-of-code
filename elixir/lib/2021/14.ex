import AOC

# https://adventofcode.com/2021/day/14
aoc 2021, 14 do
  def p1(input) do
    {template, mapping} = parse_input(input)
    expand(template, mapping, 10) |> calculate_score
  end

  def p2(input) do
    parse_input(input)
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

  # Base case - no more rounds
  def expand(template, _, 0), do: template

  @doc "Expand the template for the given number of rounds given a mapping of character pairs to what should be inserted"
  def expand([first_char | rest_char], mapping, num_rounds) do
    # Starting with the second char with the previous char as the accumulator,
    # determine what char needs to be inserted and return a list of the previous char and the inserted char.
    # Flat map to collapse into a single array.
    {template, last_char} =
      Enum.flat_map_reduce(rest_char, first_char, fn char, last_char ->
        {[last_char, mapping[{last_char, char}]], char}
      end)

    # The last char would not have been returned in the mapping - it would have been returned as the accumulator.
    # Append to the template before recursing.
    expand(template ++ [last_char], mapping, num_rounds - 1)
  end

  @doc "Calculate the difference between the most and least frequent letters."
  def calculate_score(template) do
    frequencies = template |> Enum.frequencies() |> Map.values() |> Enum.sort()
    Enum.at(frequencies, -1) - Enum.at(frequencies, 0)
  end
end

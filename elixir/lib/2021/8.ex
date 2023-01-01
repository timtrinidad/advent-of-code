import AOC

aoc 2021, 8 do
  def p1(input) do
    # Create a flat list of all sequences from the right side
    rights = parse_input(input) |> Enum.flat_map(fn {_, right} -> right end)

    # Count the number of items on the right side that have 2, 3, 4, or 7 characters
    rights
    |> Enum.count(fn x ->
      length(x) in [2, 3, 4, 7]
    end)
  end

  def p2(input) do
    parse_input(input)
    |> Enum.map(fn {left, right} ->
      # Generate a dictionary of character sequences to digits
      dictionary = generate_digit_dictionary(left)
      # Apply that dictionary to translate the right side
      right |> Enum.map(fn digit -> dictionary[MapSet.new(digit)] end) |> Integer.undigits()
    end)
    # Sum all translated right side values
    |> Enum.sum()
  end

  @doc "Split input into a list of tuples, each tuple being a list of characters"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [left, right] = String.split(line, " | ")

      {String.split(left, " ") |> Enum.map(&String.graphemes/1),
       String.split(right, " ") |> Enum.map(&String.graphemes/1)}
    end)
  end

  def generate_digit_dictionary(observations) do
    sorted = observations |> Enum.sort_by(&length/1) |> Enum.map(&MapSet.new/1)
    # The sorted-by-length list has lengths [2, 3, 4, 5, 5, 5, 6, 6, 6, 7].
    # We know lengths 2, 3, 4 and 7 correspond to numbers 1, 7, 4, and 8 respectively.
    [one, _, four, _, _, _, _, _, _, _] = sorted

    sorted
    |> Enum.reduce(%{}, fn sequence, acc ->
      num_intersection_one = MapSet.intersection(one, sequence) |> MapSet.size()
      num_intersection_four = MapSet.intersection(four, sequence) |> MapSet.size()

      # Implementation of https://www.reddit.com/r/adventofcode/comments/rbj87a/comment/hnoyy04/?utm_source=share&utm_medium=web2x&context=3
      # Original implementation at https://github.com/timtrinidad/advent-of-code/blob/08cb99f2b9391ad78501bded698d47d297574a25/elixir/lib/2021/8.ex
      number =
        case {MapSet.size(sequence), num_intersection_four, num_intersection_one} do
          {2, _, _} -> 1
          {3, _, _} -> 7
          {4, _, _} -> 4
          {7, _, _} -> 8
          {5, 2, _} -> 2
          {5, 3, 1} -> 5
          {5, 3, 2} -> 3
          {6, 4, _} -> 9
          {6, 3, 1} -> 6
          {6, 3, 2} -> 0
        end

      Map.put(acc, sequence, number)
    end)
  end
end

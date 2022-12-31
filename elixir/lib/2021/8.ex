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
    [one, seven, four, unk1, unk2, unk3, unk4, unk5, unk6, eight] = sorted
    # Digits with length 5
    length5 = [unk1, unk2, unk3]
    # Digits with legnth 6
    length6 = [unk4, unk5, unk6]

    # Six - length 6 digit that doesn't share a letter with "one"
    six = length6 |> Enum.find(fn x -> MapSet.size(MapSet.union(x, one)) == 7 end)

    # Remaining length6 is either 0 or 9. Zero must be the one that doesn't share a letter with "four"
    zero = length6 |> Enum.find(fn x -> x != six && MapSet.size(MapSet.union(x, four)) == 7 end)
    # Remaining length6 must be "nine"
    nine = length6 |> Enum.find(fn x -> x != six && x != zero end)
    # Three - length 5 digit which contains all the letters on "one"
    three = length5 |> Enum.find(fn x -> MapSet.size(MapSet.union(x, one)) == 5 end)

    # Remaining length 5 is either 5 or 9. The one which doesn't share a letter with nine must be 5
    five = length5 |> Enum.find(fn x -> x != three && MapSet.size(MapSet.union(x, nine)) == 6 end)
    # Remaining length 5 must be 2
    two = length5 |> Enum.find(fn x -> x != three && x != five end)

    dictionary = %{
      zero => 0,
      one => 1,
      two => 2,
      three => 3,
      four => 4,
      five => 5,
      six => 6,
      seven => 7,
      eight => 8,
      nine => 9
    }
  end
end

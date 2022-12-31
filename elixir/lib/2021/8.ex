import AOC

aoc 2021, 8 do
  def p1(input) do
    # Create a flat list of all sequences from the right side
    rights = parse_input(input) |> Enum.flat_map(fn {_, right} -> right end)

    # Count the number of items on the right side that have 2, 3, 4, or 7 characters
    rights
    |> Enum.count(fn x ->
      String.length(x) in [2, 3, 4, 7]
    end)
  end

  def p2(input) do
  end

  @doc "Split input into a list of tuples, each tuple being a list of characters"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [left, right] = String.split(line, " | ")
      {String.split(left, " "), String.split(right, " ")}
    end)
  end
end

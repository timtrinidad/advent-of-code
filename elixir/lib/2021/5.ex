import AOC

aoc 2021, 5 do
  def p1(input) do
    parse_input(input)
    |> Enum.filter(fn [{x1, y1}, {x2, y2}] -> x1 == x2 || y1 == y2 end)
    |> count_overlapping_points
  end

  def p2(input) do
    parse_input(input) |> count_overlapping_points
  end

  @doc "Parse the input into a list of tuple pairs"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split(" -> ")
      |> Enum.map(fn coords ->
        [x, y] = String.split(coords, ",")
        {String.to_integer(x), String.to_integer(y)}
      end)
    end)
  end

  @doc "Count the number of overlapping points for a given list of lines"
  def count_overlapping_points(lines) do
    lines
    |> Enum.flat_map(fn [{x1, y1}, {x2, y2}] ->
      if x1 == x2 || y1 == y2,
        # For horizontal or vertical lines
        do: for(x <- x1..x2, y <- y1..y2, do: {x, y}),
        # For diagonal lines
        else: Enum.zip(x1..x2, y1..y2)
    end)
    # Count frequencies of each coordinate, filter where more than 1, count number of results
    |> Enum.frequencies()
    |> Enum.filter(fn {_, freq} -> freq > 1 end)
    |> length
  end
end

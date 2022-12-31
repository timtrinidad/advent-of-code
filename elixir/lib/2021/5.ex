import AOC

aoc 2021, 5 do
  def p1(input) do
    parse_input(input)
    |> Enum.filter(fn [{x1, y1}, {x2, y2}] -> x1 == x2 || y1 == y2 end)
    |> Enum.flat_map(fn [{x1, y1}, {x2, y2}] ->
      for x <- x1..x2, y <- y1..y2, do: {x, y}
    end)
    |> Enum.frequencies()
    |> Enum.filter(fn {_, freq} -> freq > 1 end)
    |> length
  end

  def p2(input) do
  end

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
end

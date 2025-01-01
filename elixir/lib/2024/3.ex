import AOC

aoc 2024, 3 do
  def p1(input) do
    parse_input(input)
    |> Enum.map(fn
        ["mul", a, b] -> String.to_integer(a) * String.to_integer(b)
        _ -> 0
    end)
    |> Enum.sum
    |> IO.inspect
  end

  def p2(input) do
  parse_input(input)
  |> Enum.reduce({0, 1}, fn
      ["mul", a, b], {acc, on} -> {acc + String.to_integer(a) * String.to_integer(b) * on, on}
      ["do"], {acc, _} -> {acc, 1}
      ["don't"], {acc, _} -> {acc, 0}
  end)
  |> elem(0)
  |> IO.inspect
  end

  def parse_input(input) do
    input
    |> String.replace("\n", "")
    |> then(&Regex.scan(~r/(mul|do|don.t)\((?:(\d+),(\d+))?\)/, &1))
    |> Enum.map(&Enum.drop(&1, 1))
  end

end

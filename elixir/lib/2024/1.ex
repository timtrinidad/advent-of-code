import AOC

aoc 2024, 1 do
  def p1(input) do
    input
    |> parse_input
    |> Enum.map(&Enum.sort/1)
    |> List.zip
    |> Enum.reduce(0, fn {a, b}, acc -> acc + abs(a - b) end)
    |> IO.inspect
  end

  def p2(input) do
    [l1, l2] = input |> parse_input
    freqs = l2 |> Enum.frequencies
    l1
    |> Enum.reduce(0, fn x, acc -> acc + x * Map.get(freqs, x, 0) end)
    |> IO.inspect
  end

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, "   "))
    |> Enum.map(&Enum.map(&1, fn x -> String.to_integer(x) end))
    |> Enum.reduce([[], []], fn [a, b], [al, bl] -> [[a | al], [b | bl]] end)
  end
end

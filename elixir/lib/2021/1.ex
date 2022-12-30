import AOC

aoc 2021, 1 do
  def p1(input) do
    parse_input(input) |> Enum.map_reduce(9999999999999, fn x, acc ->
      {(if x > acc, do: 1, else: 0), x}
    end) |> elem(0) |> Enum.sum
    |> IO.inspect
  end

  def p2(input) do
  end

  def parse_input(input) do
    input |> String.trim |> String.split("\n") |> Enum.map(&String.to_integer/1)
  end
end

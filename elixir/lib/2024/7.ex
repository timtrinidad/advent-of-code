import AOC

aoc 2024, 7 do
  def p1(input) do
    input
      |> parse_input
      |> Enum.filter(fn {expected, [first | rest]} -> is_valid?(rest, first, expected, [:add, :multiply]) end)
      |> Enum.map(&(elem(&1, 0)))
      |> Enum.sum
      |> IO.inspect
  end

  def p2(input) do
    input
      |> parse_input
      |> Enum.filter(fn {expected, [first | rest]} -> is_valid?(rest, first, expected, [:add, :multiply, :combine]) end)
      |> Enum.map(&(elem(&1, 0)))
      |> Enum.sum
      |> IO.inspect
  end

  def parse_input(input) do
    input
        |> String.split("\n", trim: true)
        |> Enum.map(fn line -> String.split(line, [": ", " "]) |> Enum.map(&String.to_integer/1) end)
        |> Enum.map(&({hd(&1), tl(&1)}))
  end

  def is_valid?([], actual, expected, _), do: actual == expected
  def is_valid?([num | res], actual, expected, possible_ops) do
    Enum.any?(possible_ops, &is_valid?(res, apply_op(&1, actual, num), expected, possible_ops))
  end

  def apply_op(:add, a, b), do: a + b
  def apply_op(:multiply, a, b), do: a * b
  def apply_op(:combine, a, b), do: Integer.to_string(a) <> Integer.to_string(b) |> String.to_integer
end

import AOC

aoc 2021, 7 do
  def p1(input) do
    input = parse_input(input)
    {min, max} = Enum.min_max(input)

    for i <- min..max do
      input |> Enum.map(fn x -> abs(x - i) end) |> Enum.sum()
    end
    |> Enum.min()
  end

  def p2(input) do
  end

  @doc "Split input into a list of integers"
  def parse_input(input) do
    input |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
  end
end

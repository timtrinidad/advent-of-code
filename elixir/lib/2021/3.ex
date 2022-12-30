import AOC

aoc 2021, 3 do
  def p1(input) do
    input = parse_input(input)
    num_lines = length(input)
    line_length = length(input |> Enum.at(0))

    # For each column, choose 1 if sum of column is greater than half the length, otherwise 0
    gamma =
      for i <- 1..line_length do
        sum = input |> Enum.map(&Enum.at(&1, i - 1)) |> Enum.sum()
        if sum > num_lines / 2, do: 1, else: 0
      end

    # For epsilon, flip bits
    epsilon = gamma |> Enum.map(fn x -> if x == 1, do: 0, else: 1 end)
    array_to_int(gamma) * array_to_int(epsilon)
  end

  def p2(input) do
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn x -> String.graphemes(x) |> Enum.map(&String.to_integer/1) end)
  end

  def array_to_int(list) do
    list
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce(0, fn {val, pow}, acc -> acc + :math.pow(2, pow) * val end)
    |> trunc
  end
end

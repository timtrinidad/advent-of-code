import AOC

aoc 2021, 3 do
  def p1(input) do
    input = parse_input(input)
    line_length = length(input |> Enum.at(0))

    # For each column, choose 1 if sum of column is greater than half the length, otherwise 0
    gamma =
      for i <- 1..line_length do
        get_column_majority(input, i - 1)
      end

    # For epsilon, flip bits
    epsilon = gamma |> Enum.map(fn x -> if x == 1, do: 0, else: 1 end)
    [gamma, epsilon] |> Enum.map(&array_to_int/1) |> Enum.product()
  end

  def p2(input) do
    input = parse_input(input)
    num_lines = length(input)
    line_length = length(input |> Enum.at(0))

    o2 =
      1..line_length
      |> Enum.reduce_while(input, fn col, lines ->
        maj = get_column_majority(lines, col - 1) || 1
        lines = lines |> Enum.filter(fn x -> Enum.at(x, col - 1) == maj end)
        if length(lines) == 1, do: {:halt, Enum.at(lines, 0)}, else: {:cont, lines}
      end)

    co2 =
      1..line_length
      |> Enum.reduce_while(input, fn col, lines ->
        maj = get_column_majority(lines, col - 1) || 1
        lines = lines |> Enum.filter(fn x -> Enum.at(x, col - 1) != maj end)
        if length(lines) == 1, do: {:halt, Enum.at(lines, 0)}, else: {:cont, lines}
      end)

    [o2, co2] |> Enum.map(&array_to_int/1) |> Enum.product()
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn x -> String.graphemes(x) |> Enum.map(&String.to_integer/1) end)
  end

  def array_to_int(list) do
    Integer.undigits(list, 2)
  end

  def get_column_majority(list, column) do
    num_lines = length(list)
    sum = list |> Enum.map(&Enum.at(&1, column)) |> Enum.sum()

    cond do
      sum > num_lines / 2 -> 1
      sum == num_lines / 2 -> nil
      true -> 0
    end
  end
end

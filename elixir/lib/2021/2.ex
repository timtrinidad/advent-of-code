import AOC

aoc 2021, 2 do
  def p1(input) do
    {h, d} =
      parse_input(input)
      # Sum up each depth and horizontal position
      |> Enum.reduce({0, 0}, fn {h, d}, {acc_h, acc_d} -> {acc_h + h, acc_d + d} end)

    h * d
  end

  def p2(input) do
    {h, d, _} =
      parse_input(input)
      |> Enum.reduce({0, 0, 0}, fn {h, d}, {acc_h, acc_d, acc_a} ->
        {
          # Sum up each horizontal position
          acc_h + h,
          # Add to depth the current aim times forward position
          acc_d + acc_a * h,
          # Set aim to be sum of "depth" of input
          acc_a + d
        }
      end)

    h * d
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [string, num] = String.split(line, " ")
      num = String.to_integer(num)

      case string do
        "forward" -> {num, 0}
        "down" -> {0, num}
        "up" -> {0, -num}
      end
    end)
  end
end

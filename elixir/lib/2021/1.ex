import AOC

aoc 2021, 1 do
  def p1(input) do
    parse_input(input)
    |> count_increasing(1)
    |> IO.inspect()
  end

  def p2(input) do
    parse_input(input)
    |> count_increasing(3)
    |> IO.inspect()
  end

  def count_increasing(list, window_size) do
    init = for _ <- 1..window_size, do: 9_999_999_999_999

    list
    |> Enum.map_reduce(init, fn x, acc ->
      next_window = tl(acc) ++ [x]
      {if(Enum.sum(next_window) > Enum.sum(acc), do: 1, else: 0), next_window}
    end)
    |> elem(0)
    |> Enum.sum()
  end

  def parse_input(input) do
    input |> String.trim() |> String.split("\n") |> Enum.map(&String.to_integer/1)
  end
end

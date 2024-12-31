import AOC

aoc 2024, 2 do
  def p1(input) do
    parse_input(input)
    |> Enum.map(&is_safe/1)
    |> Enum.sum
    |> IO.inspect
  end

  def p2(input) do
    parse_input(input)
    |> Enum.map(fn line ->
        (is_safe(line) == 1 || can_be_safe(line)) && 1 || 0
    end)
    |> Enum.sum
    |> IO.inspect
  end

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " "))
    |> Enum.map(&Enum.map(&1, fn x -> String.to_integer(x) end))
  end

  def is_safe(line) do
    pairs = Enum.zip(line, tl(line))
    decreasing = Enum.all?(pairs, &elem(&1, 1) < elem(&1, 0))
    increasing = Enum.all?(pairs, &elem(&1, 1) > elem(&1, 0))
    noGaps = Enum.all?(pairs, &abs(elem(&1, 0) - elem(&1, 1)) <= 3)
    (increasing || decreasing) && noGaps && 1 || 0
  end

  def can_be_safe(line) do
    0..(length(line)-1)
    |> Enum.any?(fn i ->
      line
      |> Enum.with_index()
      |> Enum.reject(fn {_, idx} -> idx == i end)
      |> Enum.map(&elem(&1, 0))
      |> is_safe == 1
    end)
  end
end

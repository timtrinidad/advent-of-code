import AOC

aoc 2024, 5 do
  def p1(input) do
    {order, lists} = input |> parse_input
    lists
        |> Enum.filter(&is_valid?(&1, order))
        |> sum_middles
        |> IO.inspect
  end

  def p2(input) do
      {order, lists} = input |> parse_input
      lists
        |> Enum.reject(&is_valid?(&1, order))
        |> Enum.map(fn list -> Enum.sort(list, &(MapSet.member?(order, {&1, &2}))) end)
        |> sum_middles
        |> IO.inspect
  end

  def parse_input(input) do
    [order, lists] = input
        |> String.split("\n\n", trim: true)
    lists = lists
        |> String.split("\n")
        |> Enum.map(&String.split(&1, ","))
    order = order
        |> String.split("\n")
        |> Enum.map(&String.split(&1, "|"))
        |> Enum.map(&List.to_tuple/1)
        |> MapSet.new
    {order, lists}
  end

  def is_valid?(list, order) do
    list |> Enum.zip(tl(list)) |> Enum.all?(fn {a, b} ->
      !MapSet.member?(order, {b, a})
    end)
  end

  def get_middle(list) do
    Enum.at(list, list |> length() |> div(2))
  end

  def sum_middles(lists) do
    lists
      |> Enum.map(&get_middle/1)
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum
  end
end

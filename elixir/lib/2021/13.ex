import AOC

# https://adventofcode.com/2021/day/13
aoc 2021, 13 do
  def p1(input) do
    # Fold based on the first instruction and get number of remaining dots in the set
    {dots, instructions} = parse_input(input)
    dots |> fold(Enum.at(instructions, 0)) |> MapSet.size()
  end

  def p2(input) do
    # Fold based on all instructions and render final points
    {dots, instructions} = parse_input(input)

    instructions
    |> Enum.reduce(dots, fn instruction, dots ->
      fold(dots, instruction)
    end)
    |> render

    :ok
  end

  @doc "Split the input into a mapset of {x, y} coordinates and a list of {x or y, val} fold instructions"
  def parse_input(input) do
    [dots, instructions] = input |> String.trim() |> String.split("\n\n")

    dots =
      dots
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn line ->
        [x, y] = line |> String.split(",") |> Enum.map(&String.to_integer/1)
        {x, y}
      end)
      |> MapSet.new()

    instructions =
      instructions
      |> String.split("\n")
      |> Enum.map(fn line ->
        [axis, val] = line |> String.replace("fold along ", "") |> String.split("=")
        {axis, String.to_integer(val)}
      end)

    {dots, instructions}
  end

  @doc "For a given fold instruction, determine where points greater than the given value would fall after a fold."
  def fold(dots, {axis, val}) do
    case axis do
      "y" ->
        Enum.map(dots, fn {x, y} ->
          new_y = if y > val, do: 2 * val - y, else: y
          {x, new_y}
        end)

      "x" ->
        Enum.map(dots, fn {x, y} ->
          new_x = if x > val, do: 2 * val - x, else: x
          {new_x, y}
        end)
    end
    |> MapSet.new()
  end

  @doc "Render all dots based on the min/max x/y in the set"
  def render(dots) do
    {min_x, max_x} = dots |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
    {min_y, max_y} = dots |> Enum.map(&elem(&1, 1)) |> Enum.min_max()
    IO.puts("\n\n")

    for y <- min_y..max_y do
      for x <- min_x..max_x do
        if MapSet.member?(dots, {x, y}), do: IO.write("█"), else: IO.write(" ")
      end

      IO.puts("")
    end
  end
end

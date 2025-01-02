import AOC

aoc 2024, 4 do
  @dirs8 for dx <- -1..1,
             dy <- -1..1,
             {dx, dy} != {0, 0},
             do: {dx, dy}

  @dirs4 for dx <- [-1, 1],
             dy <- [-1, 1],
             do: {dx, dy}

  def p1(input) do
    grid = parse_input(input)
    for {{x, y}, ?X} <- grid, {dx, dy} <- @dirs8 do
        Enum.map(0..3, fn i -> grid[{x + dx * i, y + dy * i}] end)
    end
    |> Enum.count(&(&1 == ~c"XMAS"))
    |> IO.inspect

  end

  def p2(input) do
    grid = parse_input(input)
    # For every 'A' in the grid, get its 4 corners
    for {{x, y}, ?A} <- grid do
        @dirs4 |> Enum.map(fn {dx, dy} -> grid[{x + dx, y + dy}] end)
    end
    # If the 4 corners are in a valid pattern, count it. Don't count ones
    # where opposite corners are th same letter (e.g. MSSM)
    |> Enum.count(&(&1 in [~c"MMSS", ~c"SSMM", ~c"SMSM", ~c"MSMS"]))
    |> IO.inspect
  end

  def parse_input(input) do
    for {line, y} <- input |> String.split("\n", trim: true) |> Enum.with_index,
        {char, x} <- line |> String.to_charlist |> Enum.with_index,
        into: %{},
        do: {{x, y}, char}
  end
end

import AOC

# https://adventofcode.com/2022/day/10
aoc 2022, 10 do
  def p1(input) do
    {history, _} = generate_cycle_history(input)
    frequency_cycles = [20, 60, 100, 140, 180, 220]

    for n <- frequency_cycles do
      history |> Enum.at(n - 1) |> Kernel.*(n)
    end
    |> Enum.sum()
    |> IO.inspect(
      label: "Sum of register values at frequencies [#{Enum.join(frequency_cycles, ", ")}]"
    )

    render(history)
  end

  def p2(_) do
    IO.puts("See part 1")
  end

  @doc "Generate a history of register values for each cycle"
  def generate_cycle_history(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn x ->
      # Break each command into separate increment values for each cycle
      case x |> String.split(" ") do
        # addx is 2 cycles (update after the second)
        ["addx", num] -> [0, String.to_integer(num)]
        # noop does not increment and consumes 1 cycle
        ["noop"] -> [0]
      end
    end)
    |> Enum.concat()
    # At this point we have a flat list of cycles (e.g. [0, 0, -15, 0, 5, 0, 0, 0, 2]
    |> Enum.map_reduce(1, fn x, acc ->
      # Because register is not updated until AFTER cycle, store
      # map value from before the increment
      post_cycle_val = acc + x
      {acc, post_cycle_val}
    end)
  end

  @doc "Given a cycle history, render pixels on a screen"
  def render(history) do
    history
    |> Enum.with_index()
    |> Enum.map(fn {sprite_pos, idx} ->
      screen_width = 40
      # idx is the cycle number
      # column is mod 40 (screen width)
      col = rem(idx, screen_width)
      if col == 0, do: IO.write('\n')

      pixel =
        cond do
          # If the given col is within the sprite pos +/- 1, render light (#)
          col >= sprite_pos - 1 and col <= sprite_pos + 1 -> '#'
          # Otherwise render dark (.)
          true -> '.'
        end

      IO.write(pixel)
    end)

    IO.puts("")
  end
end

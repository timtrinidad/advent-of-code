# https://adventofcode.com/2022/day/5

defmodule DayFive do
  @doc "parse inputs and move crates"
  def move_stacks(input, type) do
    [stacks_input, moves_input] = input
      |> String.trim
      |> String.split("\n\n")

    stacks = parse_stacks(stacks_input)
    moves = parse_moves(moves_input, type)
    handle_moves(stacks, moves)
  end

  @doc "Parse the top of the input - the starting stack state"
  def parse_stacks(input) do
    {base, crates} = input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(&String.graphemes/1)
      |> List.pop_at(-1)
    # analyze each string column. If a number is found, grab the letters in that same column.
    base |> Enum.with_index |> Enum.reduce([], fn {x, idx}, acc ->
      if x == " ", do: acc, else: acc ++ [Enum.map(crates, fn row ->
          row |> Enum.at(idx) end) |> Enum.filter(fn x -> x != " " end)]
    end)
  end

  @doc "Parse the list of moves"
  def parse_moves(input, type) do
    input
      |> String.trim
      |> String.split("\n")
      |> Enum.flat_map(fn x ->
        [_, num, _, from, _, to] = String.split(x, " ")
        from = String.to_integer(from)
        to = String.to_integer(to)
        num = String.to_integer(num)
        case type do
          # Repeat each move based on number of creates
          :mover_9000 -> for _ <- 1..num, do: {1, from, to}
          # Each move can handle num crates
          :mover_9001 -> [{num, from, to}]
        end
      end)
  end

  @doc "Modify the stack based on the list of moves"
  def handle_moves(stacks, [{num, from, to} | next_moves]) do
    # Remove from stack
    {items, from_stack} = Enum.at(stacks, from - 1) |> Enum.split(num)
    # Add to stack
    to_stack = items ++ Enum.at(stacks, to - 1)
    # Return individual stacks to list of stacks
    stacks = stacks
      |> List.replace_at(from - 1, from_stack)
      |> List.replace_at(to - 1, to_stack)

    handle_moves(stacks, next_moves)
  end
  # No more moves - return current stack
  def handle_moves(stacks, []), do: stacks
end

{:ok, input} = File.read('2022/05/input.txt')

for n <- [:mover_9000, :mover_9001], do: DayFive.move_stacks(input, n)
  |> Enum.map(fn x -> Enum.at(x, 0) end)
  |> Enum.join
  |> IO.inspect(label: "Top of each stack with #{n}")

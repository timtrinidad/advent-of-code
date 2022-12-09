# https://adventofcode.com/2022/day/9

defmodule DayNine do
  @doc "Generate a history of coordinates for head and tails based on move list"
  def get_move_and_coordinates_history(input, num_tails) do
    # Convert input into an array of tuples for {dir,num}
    moves = input |> String.trim |> String.split("\n")
      |> Enum.map(fn x ->
        [dir, num] = x |> String.split(" ")
        num = String.to_integer(num)
        {dir, num}
      end)
    # Initialize coordinates for each of the tails
    tails_pos = for _ <- 1..num_tails, do: {0, 0}
    # Start with current move
    {curr_move, next_moves} = List.pop_at(moves, 0)
    handle_direction(curr_move, next_moves, {0, 0}, tails_pos, [])
   end

  @doc """
    Recursive function to determine new position of head and each tail based on current move.
    Recursively runs until direction hits 0 (e.g. for "R 2", recurses to "R 1" and "R 0" before effecting next move)
    Returns a history of each move and coordinates.
  """
  def handle_direction({dir, num}, next_moves, head_pos, tails_pos, history) do
    cond do
      # BASE CASE: If current move is exhausted (num is 0) and no more next moves, return history
      num == 0 && length(next_moves) == 0 -> history
      # If current move is exhausted, get the next move (into curr_move) and recurse
      num == 0 ->
        [curr_move | next_moves] = next_moves;
        handle_direction(curr_move, next_moves, head_pos, tails_pos, history)
      # Determine new position of head and tails
      true ->
        {head_pos, tails_pos} = move(dir, head_pos, tails_pos)
        new_history_entry = %{
          :curr_move => {dir, num},
          :head => head_pos,
          :tails => tails_pos
        }
        handle_direction(
          # Each call only moves one spot - recurse again with one fewer number of spots to move
          {dir, num - 1},
          next_moves,
          head_pos,
          tails_pos,
          # Add a new entry to the history
          history ++ [new_history_entry]
        )
    end
   end

   @doc "Determine position of head and each of the tails based on the direction (L/R/U/D). Will only move one spot."
   def move(dir, head_pos, tails_pos) do
    {head_x, head_y} = head_pos
    # Update head coordinates based on direction
    head_pos = case dir do
      "L" -> {head_x - 1, head_y}
      "R" -> {head_x + 1, head_y}
      "U" -> {head_x, head_y + 1}
      "D" -> {head_x, head_y - 1}
    end

    # For each of the tails, update the position based on the preceeding knot, starting with the head
    {tails_pos, _} = tails_pos |> Enum.map_reduce(head_pos, fn tail_pos, head_pos ->
      {tail_x, tail_y} = tail_pos
      delta_x = elem(head_pos, 0) - elem(tail_pos, 0)
      delta_y = elem(head_pos, 1) - elem(tail_pos, 1)
      dist_sq = :math.pow(delta_x, 2) + :math.pow(delta_y, 2)

      tail_pos = case {delta_x, delta_y, dist_sq} do
        # If knots are adjacent, delta will at most be {1, 1} which means dist = sqrt(1^2 + 1^2);
        # dist_sq will be at most 2. If less than 2, this means knots are adjacent (incl diagonal).
        # Do nothing.
        {_, _, dist} when dist <= 2 -> tail_pos
        # Movement in y direction only
        {0, y, _} -> {tail_x, tail_y + (if y > 0, do: 1, else: -1)}
        # Movement in x direction only
        {x, 0, _} -> {tail_x + (if x > 0, do: 1, else: -1), tail_y}
        # Movement in both directions - move diagonally
        {x, y, _} -> {tail_x + (if x > 0, do: 1, else: -1), tail_y + (if y > 0, do: 1, else: -1)}
      end
      {tail_pos, tail_pos}
    end)
    {head_pos, tails_pos}
   end
end

{:ok, input} = File.read('2022/09/input.txt')

for num_tails <- [1, 9] do
  history = DayNine.get_move_and_coordinates_history(input, num_tails)
  history
#    |> IO.inspect(limit: :infinity)
    |> Enum.map(fn x -> x.tails |> Enum.at(-1) end)
    |> MapSet.new
#    |> IO.inspect(limit: :infinity)
    |> MapSet.size
    |> IO.inspect(label: "# unique coordinates for last tail with #{num_tails} tail(s)")
end

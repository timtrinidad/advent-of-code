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
          :moves_remaining => length(next_moves),
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

   def visualize(history) do
    edges = history |> Enum.reduce(%{:min=>%{:x=>0,:y=>0}, :max=>%{:x=>0,:y=>0}}, fn entry, acc ->
      {head_x, head_y} = entry[:head]
      acc = put_in(acc[:min][:x], min(acc[:min][:x], head_x))
      acc = put_in(acc[:max][:x], max(acc[:max][:x], head_x))
      acc = put_in(acc[:min][:y], min(acc[:min][:y], head_y))
      put_in(acc[:max][:y], max(acc[:max][:y], head_y))
    end)
    history |> Enum.map(fn entry ->
      points = entry[:tails] |> Enum.with_index |> Enum.reduce(['H': entry.head], fn {tail, idx_point}, acc ->
        Keyword.put(acc, :"#{idx_point + 1}", tail)
      end) |> Enum.reverse
      grid_height = edges[:max][:y] - edges[:min][:y]
      grid_width = edges[:max][:x] - edges[:min][:x]
      IO.puts("\n\n\n\n\n\n\n#{entry.moves_remaining}")
      grid = for curr_y <- 0..grid_height do
        for curr_x <- 0..grid_width do
          point = Enum.find(points, {:., nil}, fn x -> elem(x, 1) == {curr_x + edges[:min][:x], curr_y + edges[:min][:y]} end) |> elem(0)
          Atom.to_charlist(point) |> Enum.at(0)
        end
      end
      grid |> Enum.map(&IO.puts/1)
      :timer.sleep(80)
    end)
   end
end

{:ok, input} = File.read('2022/09/input.txt')

history = DayNine.get_move_and_coordinates_history(input, 9)
history
  |> Enum.map(fn x -> x.tails |> Enum.at(0) end)
  |> MapSet.new |> MapSet.size
  |> IO.inspect(label: "# unique coordinates for tail # 1")
history
  |> Enum.map(fn x -> x.tails |> Enum.at(8) end)
  |> MapSet.new |> MapSet.size
  |> IO.inspect(label: "# unique coordinates for tail # 9")

DayNine.visualize(history)
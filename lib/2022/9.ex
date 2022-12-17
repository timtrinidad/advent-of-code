import AOC

# https://adventofcode.com/2022/day/9
aoc 2022, 9 do
  def p1(input) do
    get_move_and_coordinates_history(input, 1)
    |> Enum.map(fn x -> x.tails |> List.last() end)
    |> MapSet.new()
    |> MapSet.size()
    |> IO.inspect(label: "# unique coordinates for tail #1")
  end

  def p2(input) do
    get_move_and_coordinates_history(input, 9)
    |> Enum.map(fn x -> x.tails |> List.last() end)
    |> MapSet.new()
    |> MapSet.size()
    |> IO.inspect(label: "# unique coordinates for tail #9")
  end

  @doc "Generate a history of coordinates for head and tails based on move list"
  def get_move_and_coordinates_history(input, num_tails) do
    # Convert input into an array of tuples for {dir,num}
    moves =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn x ->
        [dir, num] = x |> String.split(" ")
        for _ <- 1..String.to_integer(num), do: dir
      end)
      |> Enum.concat()

    # Initialize coordinates for each of the tails
    tails_pos = for _ <- 1..num_tails, do: {0, 0}
    handle_direction(moves, {0, 0}, tails_pos, [])
  end

  @doc """
    Recursive function to determine new position of head and each tail based on current move.
    Recursively runs until no more moves left.
    Returns a history of each move and coordinates.
  """
  def handle_direction([dir | next_moves], head_pos, tails_pos, history) do
    {head_pos, tails_pos} = move(dir, head_pos, tails_pos)
    # Add a new entry to the history
    history =
      history ++
        [
          %{
            :curr_move => dir,
            :head => head_pos,
            :tails => tails_pos
          }
        ]

    if length(next_moves) > 0 do
      # Recurse with remaining moves
      handle_direction(
        next_moves,
        head_pos,
        tails_pos,
        history
      )
    else
      # Base case - no more moves
      history
    end
  end

  @doc "Determine position of head and each of the tails based on the direction (L/R/U/D). Will only move one spot."
  def move(dir, head_pos, tails_pos) do
    {head_x, head_y} = head_pos
    # Update head coordinates based on direction
    head_pos =
      case dir do
        "L" -> {head_x - 1, head_y}
        "R" -> {head_x + 1, head_y}
        "U" -> {head_x, head_y + 1}
        "D" -> {head_x, head_y - 1}
      end

    # For each of the tails, update the position based on the preceeding knot, starting with the head
    {tails_pos, _} =
      tails_pos
      |> Enum.map_reduce(head_pos, fn tail_pos, head_pos ->
        {tail_x, tail_y} = tail_pos
        delta_x = elem(head_pos, 0) - elem(tail_pos, 0)
        delta_y = elem(head_pos, 1) - elem(tail_pos, 1)
        dist_sq = :math.pow(delta_x, 2) + :math.pow(delta_y, 2)

        tail_pos =
          case {delta_x, delta_y, dist_sq} do
            # If knots are adjacent, delta will at most be {1, 1} which means dist = sqrt(1^2 + 1^2);
            # dist_sq will be at most 2. If less than 2, this means knots are adjacent (incl diagonal).
            # Do nothing.
            {_, _, dist} when dist <= 2 ->
              tail_pos

            # Movement in y direction only
            {0, y, _} ->
              {tail_x, tail_y + if(y > 0, do: 1, else: -1)}

            # Movement in x direction only
            {x, 0, _} ->
              {tail_x + if(x > 0, do: 1, else: -1), tail_y}

            # Movement in both directions - move diagonally
            {x, y, _} ->
              {tail_x + if(x > 0, do: 1, else: -1), tail_y + if(y > 0, do: 1, else: -1)}
          end

        {tail_pos, tail_pos}
      end)

    {head_pos, tails_pos}
  end
end

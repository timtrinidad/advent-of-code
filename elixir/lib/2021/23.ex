import AOC

# https://adventofcode.com/2021/day/23
aoc 2021, 23 do
  def p1(input) do
    :ets.new(:cache, [:set, :named_table])

    parse_input(input)
    |> emulate(MapSet.new(), [])
  end

  def p2(input) do
    parse_input(input)
  end

  @doc "Parse the input into a map of letter or open spaces, keyed by {x, y}"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, acc ->
        case char do
          "#" -> acc
          " " -> acc
          "." -> Map.put(acc, {x, y}, char)
          char -> Map.put(acc, {x, y}, char)
        end
      end)
    end)
  end

  @doc "DFS to find lowest score that gets all letters in the right spot"
  def emulate(map, seen, history) do
    # Memoization
    cache = :ets.lookup(:cache, map)
    cached_score = if length(cache) > 0, do: elem(Enum.at(cache, 0), 1), else: nil

    # Ensure we don't repeat a move
    seen = MapSet.put(seen, map)

#    IO.puts(history |> Enum.reverse() |> Enum.join(""))

    cond do
      # This specific state is memoized - return the solved problem subset
      cached_score != nil ->
        cached_score

      # Base case - map is done - no more to do
      is_done(map) ->
        0

      # Explore each possibility and return the lowest score.
      # Memoize the lowest score achieved for the current map state.
      true ->
        # Find all possible next maps from the current state
        next_maps = find_next_maps(map, seen)

        # Recurse to find lowest score from all next maps
        scores =
          next_maps
          |> Enum.map(fn {map, cost, _, curr} ->
#            history = [map[curr] | history]
            cost + emulate(map, seen, history)
          end)

        # If no scores, we're stuck. Return super high score.
        score = if length(scores) > 0, do: Enum.min(scores), else: 999_999_999

        # Memoize
        :ets.insert(:cache, {map, score})
        score
    end
  end

  @doc "Determine if the map is in the done state"
  def is_done(map) do
    {map[{3, 2}], map[{3, 3}]} == {"A", "A"} &&
      {map[{5, 2}], map[{5, 3}]} == {"B", "B"} &&
      {map[{7, 2}], map[{7, 3}]} == {"C", "C"} &&
      {map[{9, 2}], map[{9, 3}]} == {"D", "D"}
  end

  @doc "Go through each point in the map and determine a list of possible moves for that point"
  def find_next_maps(map, seen) do
    moves =
      map
      |> Enum.reduce([], fn {xy, char}, acc ->
        if char == "." do
          # Do nothing - can't move an empty space
          acc
        else
          # Find all possible moves for the given space along with the cost of that move
          possible_moves(map, xy, MapSet.new(), 0)
          |> Enum.reduce(acc, fn {dest, dist}, acc ->
            [{xy, dest, move_cost(char, dist)} | acc]
          end)
        end
      end)
      # Filter out moves that aren't possible based on rules
      |> filter_invalid_moves(map)

    # Find if we have the chance to put a letter in the right spot. If so, take it
    correct =
      moves
      |> Enum.find(fn {curr, {dx, dy}, _} ->
        char = map[curr]

        cond do
          # Destination is bottom of room and is correct x
          dy == 3 &&
              ((char == "A" && dx == 3) || (char == "B" && dx == 5) || (char == "C" && dx == 7) ||
                 (char == "D" && dx == 9)) ->
            true

          # Destination is second-from-bottom of room and matches letter below it
          # Don't have to worry about the letter below it being incorrect since that
          # move would have been filtered out as invalid
          dy == 2 && char == map[{dx, 3}] ->
            true

          # If not match above two cases, no moves for letter into correct space
          true ->
            false
        end
      end)

    moves = if correct == nil, do: moves, else: [correct]

    # Convert the move (src, dest, cost) into a full {map, cost} if the new map hasn't already been seen
    # in this branch
    moves
    |> Enum.reduce([], fn {curr, dest, cost}, acc ->
      map = map |> Map.put(curr, ".") |> Map.put(dest, map[curr])
      if MapSet.member?(seen, map), do: acc, else: [{map, cost, curr, dest} | acc]
    end)
  end

  @doc "Explore all possible moves for the given letter at the position {x, y}"
  def possible_moves(map, {x, y}, seen, distance) do
    seen = MapSet.put(seen, {x, y})

    # Find all engihbors to find empty spots
    [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
    |> Enum.flat_map(fn {dx, dy} ->
      neighbor_pos = {x + dx, y + dy}

      if map[neighbor_pos] == "." && neighbor_pos not in seen,
        # Add neighbor to list of possible moves
        do:
          [{neighbor_pos, distance + 1}] ++ possible_moves(map, neighbor_pos, seen, distance + 1),
        # Base case - No more places to explore
        else: []
    end)
  end

  @doc "Given a list of all possible moves within a space, remove ones which violate rules"
  def filter_invalid_moves(moves, map) do
    moves
    |> Enum.filter(fn {curr = {curr_x, curr_y}, dest = {dest_x, dest_y}, _} ->
      curr_char = map[curr]
      # Can't move to the space outside a room
      dest_is_right_outside_room = dest in [{3, 1}, {5, 1}, {7, 1}, {9, 1}]

      # If in a hallway, can onlhy move back into a room, not into another spot in the hallway
      moving_from_hallway_to_hallway = dest_y == 1 && curr_y == 1

      # Don't allow moving a single space within a single room
      moving_into_same_room = curr_x == dest_x

      # Can't move into a room that already has another incorrect letter
      invalid_room =
        curr_y == 1 && dest_y != 1 &&
          [map[{dest_x, 2}], map[{dest_x, 3}]] not in [[".", "."], [".", curr_char]]

      # Don't move a letter which is already in the correct room
      already_in_right_spot =
        ((curr_char == "A" && curr_x == 3) || (curr_char == "B" && curr_x == 5) ||
           (curr_char == "C" && curr_x == 7) || (curr_char == "D" && curr_x == 9)) &&
          (curr_y == 3 || (curr_y == 2 && map[{curr_x, 3}] == curr_char))

      # Don't allow moving into the upper part of a room
      has_empty_spot_below = dest_y == 2 && map[{dest_x, 3}] == "."

      !dest_is_right_outside_room && !moving_from_hallway_to_hallway && !moving_into_same_room &&
        !invalid_room &&
        !already_in_right_spot && !has_empty_spot_below
    end)
  end

  @doc "Calculate the cost of moving a char the given distance"
  def move_cost(char, distance) do
    distance *
      case char do
        "A" -> 1
        "B" -> 10
        "C" -> 100
        "D" -> 1000
      end
  end

  def render(map, prev, curr) do
    for y <- 1..3 do
      for x <- 1..11 do
        char = Map.get(map, {x, y}, " ")

        char =
          cond do
            {x, y} == prev -> "█"
            {x, y} == curr -> char
            true -> String.downcase(char)
          end

        IO.write(char)
      end

      IO.puts("")
    end
  end
end

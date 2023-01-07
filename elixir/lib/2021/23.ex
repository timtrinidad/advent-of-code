import AOC

# https://adventofcode.com/2021/day/23
aoc 2021, 23 do
  @home_cols %{"A" => 3, "B" => 5, "C" => 7, "D" => 9}

  def p1(input) do
    :ets.new(:cache, [:set, :named_table])

    parse_input(input)
    |> emulate(MapSet.new(), [])
  end

  def p2(input) do
    :ets.new(:cache, [:set, :named_table])
    map = parse_input(input)

    to_insert = %{
      {3, 3} => "D",
      {3, 4} => "D",
      {3, 5} => map[{3, 3}],
      {5, 3} => "C",
      {5, 4} => "B",
      {5, 5} => map[{5, 3}],
      {7, 3} => "B",
      {7, 4} => "A",
      {7, 5} => map[{7, 3}],
      {9, 3} => "A",
      {9, 4} => "C",
      {9, 5} => map[{9, 3}]
    }

    map = Map.merge(map, to_insert)
    emulate(map, MapSet.new(), [])
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
    cache = :ets.lookup(:cache, map_cache_key(map))
    cached_score = if length(cache) > 0, do: elem(Enum.at(cache, 0), 1), else: nil

    # Ensure we don't repeat a move
    seen = MapSet.put(seen, map)

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
          |> Enum.map(fn {map, cost, _, _} ->
            cost + emulate(map, seen, history)
          end)

        # If no scores, we're stuck. Return super high score.
        score = if length(scores) > 0, do: Enum.min(scores), else: 999_999_999

        # Memoize
        :ets.insert(:cache, {map_cache_key(map), score})
        score
    end
  end

  @doc "Determine if the map is in the done state"
  def is_done(map) do
    map
    |> Enum.reduce_while(true, fn {{x, _}, char}, _ ->
      if char == "." || x == @home_cols[char], do: {:cont, true}, else: {:halt, false}
    end)
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
          poss_moves =
            possible_moves(map, xy, MapSet.new(), 0)
            |> Enum.map(fn {dest, dist} ->
              {xy, dest, move_cost(char, dist)}
            end)

          acc ++ poss_moves
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
          dy != 1 && !Map.has_key?(map, {dx, dy + 1}) &&
              (char != "." && dx == @home_cols[char]) ->
            true

          # Destination matches letter below it
          # Don't have to worry about the letter below it being incorrect since that
          # move would have been filtered out as invalid
          dy != 1 && char == map[{dx, dy + 1}] ->
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

      cond do
        # Can't move to the space outside a room
        dest in [{3, 1}, {5, 1}, {7, 1}, {9, 1}] ->
          false

        # If in a hallway, can only move back into a room, not into another spot in the hallway
        dest_y == 1 && curr_y == 1 ->
          false

        # Don't allow moving a single space within a single room
        curr_x == dest_x ->
          false

        # Can't move into a room that already has another incorrect letter
        dest_y != 1 &&
            length(
              Enum.filter(map, fn {{x, _}, char} ->
                x == @home_cols[curr_char] && char not in [".", curr_char]
              end)
            ) > 0 ->
          false

        # Can't move into a room to which it doesn't belong
        dest_y != 1 && dest_x != @home_cols[curr_char] ->
          false

        # Don't move a letter which is already in the correct room
        curr_char != "." && curr_x == @home_cols[curr_char] &&
            (!Map.has_key?(map, {curr_x, curr_y + 1}) || map[{curr_x, curr_y + 1}] == curr_char) ->
          false

        # Don't allow moving into the upper part of a room
        dest_y > 1 && map[{dest_x, dest_y + 1}] == "." ->
          false

        true ->
          true
      end
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

  def map_cache_key(map) do
    Enum.filter(map, fn {_, char} -> char != "." end)
    |> Enum.map(fn {xy, char} ->
      char =
        case char do
          "A" -> 1
          "B" -> 2
          "C" -> 3
          "D" -> 4
        end

      {xy, char}
    end)
  end

  def render(map, prev, curr) do
    for y <- 1..5 do
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

  def render(map), do: render(map, {0, 0}, {0, 0})
end

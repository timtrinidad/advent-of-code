# https://adventofcode.com/2021/day/12
import AOC

aoc 2021, 12 do
  def p1(input) do
    parse_input(input) |> find_paths("start", MapSet.new(), [], []) |> length
  end

  def p2(input) do
    parse_input(input)
  end

  # Base case - reached "end". Add "end" to the current path and add to the list of paths
  def find_paths(_, "end", seen, path, paths), do: [["end" | path] | paths]

  def find_paths(edges, curr, seen, path, paths) do
    possible_destinations = edges[curr]
    # Add to "seen" only if not capital letters
    seen = if String.match?(curr, ~r/[A-Z]+/), do: seen, else: MapSet.put(seen, curr)

    if length(possible_destinations) == 0 do
      # No more possible destinations - return paths as is without adding current (invalid) path
      paths
    else
      # For each possible destination that's not in "seen", explore that path
      possible_destinations
      |> Enum.filter(fn x -> !MapSet.member?(seen, x) end)
      |> Enum.flat_map(fn to ->
        find_paths(edges, to, seen, [curr | path], paths)
      end)
    end
  end

  @doc "Return a map of possible destinations keyed by vertex"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      [from, to] = String.split(line, "-")

      # Add to map both directions
      acc
      |> Map.update(from, [to], fn x -> [to | x] end)
      |> Map.update(to, [from], fn x -> [from | x] end)
    end)
  end
end

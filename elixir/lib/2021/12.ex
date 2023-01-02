import AOC

# https://adventofcode.com/2021/day/12
aoc 2021, 12 do
  def p1(input) do
    parse_input(input) |> find_paths("start", false, [], []) |> length
  end

  def p2(input) do
    parse_input(input) |> find_paths("start", true, [], []) |> length
  end

  # Base case - reached "end". Add "end" to the current path and add to the list of paths
  def find_paths(_, "end", _, path, paths), do: [["end" | path] | paths]

  def find_paths(edges, curr, allow_mult_small?, path, paths) do
    possible_destinations =
      edges[curr]
      |> Enum.filter(fn x ->
        is_large?(x) || allow_mult_small? || x not in path
      end)

    # For each possible destination that's not in "seen", explore that path
    possible_destinations
    |> Enum.flat_map(fn to ->
      allow_mult_small? = allow_mult_small? && (is_large?(to) || to not in path)
      find_paths(edges, to, allow_mult_small?, [curr | path], paths)
    end)
  end

  @doc "Return a map of possible destinations keyed by vertex"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      [from, to] = String.split(line, "-")

      # Add to map both directions
      acc = if to != "start", do: Map.update(acc, from, [to], fn x -> [to | x] end), else: acc
      acc = if from != "start", do: Map.update(acc, to, [from], fn x -> [from | x] end), else: acc
      acc
    end)
  end

  def is_large?(cave), do: String.match?(cave, ~r/[A-Z]+/)
end

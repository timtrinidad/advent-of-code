# https://adventofcode.com/2022/day/8

defmodule DayEight do
  @doc "Get a tree map (2D list) based on the input of type :visible or :score"
  def get_tree_map(input, type) do
    map = input
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn x -> String.graphemes(x) |> Enum.map(&String.to_integer/1) end)

    map_width = length(Enum.at(map, 0))-1
    map_height = length(map)-1
    for x <- 0..map_width do
      for y <- 0..map_height do
        case {x, y} do
          # Edge cases 😂
          # For visibility analysis, edges are always visible
          # For score analysis, edges are always 0
          {0, _} -> if type == :visible, do: 1, else: 0
          {_, 0} -> if type == :visible, do: 1, else: 0
          {^map_width, _} -> if type == :visible, do: 1, else: 0
          {_, ^map_height} -> if type == :visible, do: 1, else: 0
          # Non edge trees
          {x, y} -> get_tree_score(type, x, y, map)
        end
      end
    end
  end

  @doc "Determine the score/visibility for a given tree"
  def get_tree_score(type, x, y, map) do
    row = map |> Enum.at(y)
    column = map |> Enum.map(fn row -> Enum.at(row, x) end)
    curr = map |> Enum.at(y) |> Enum.at(x)
    trees_directions = [
      row |> Enum.take(x) |> Enum.reverse, # left
      row |> Enum.take(-(length(row)-x-1)),  # right
      column |> Enum.take(y) |> Enum.reverse, # above
      column |> Enum.take(-(length(column)-y-1)) # below
    ]

    case type do
      # Check whether or not this tree is visible from the edge
      :visible ->
        min_direction = trees_directions
        # Get the max tree height in each direction
        |> Enum.map(&Enum.max/1)
        # Get the lowest direction
        |> Enum.min
        # If the current tree is higher than the lowest direction, it's visible
        if curr > min_direction, do: 1, else: 0

      # Calculate the score based on # of trees visible
      :score -> trees_directions
        # For each direction determine the number of trees until view is blocked (or edge encountered)
        |> Enum.map(fn x -> num_trees_until_blocked(x, curr, 0) end)
        # Multiply each direction's number
        |> Enum.reduce(fn x, acc -> x * acc end)
    end
  end

  @doc "Recursive function to determine the number of trees visible for a list of trees"
  def num_trees_until_blocked(list, tree_height, num_trees) do
    {next, list} = List.pop_at(list, 0)
    cond do
      length(list) == 0 -> num_trees + 1
      tree_height > next -> num_trees_until_blocked(list, tree_height, num_trees + 1)
      true -> num_trees + 1
    end
  end

end

{:ok, input} = File.read('2022/08/input.txt')

visible_trees = DayEight.get_tree_map(input, :visible)
#visible_trees |> Enum.map(fn x -> Enum.join(x, "") |> IO.puts end)
num_visible_trees = visible_trees |> Enum.map(&Enum.sum/1) |> Enum.sum
IO.inspect(num_visible_trees, label: "# Visible Trees")

tree_scores = DayEight.get_tree_map(input, :score)
#tree_scores |> Enum.map(fn x -> Enum.join(x, ",") |> IO.puts end)
max_score = tree_scores |> Enum.map(&Enum.max/1) |> Enum.max
IO.inspect(max_score, label: "Max score")

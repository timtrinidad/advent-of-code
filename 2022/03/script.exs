# https://adventofcode.com/2022/day/3

defmodule DayThree do
  @doc "Get a list of priorities for each group based on type (:half or :three)"
  def getPriorities(input, type) do
    getGroups(input, type)
      |> Enum.map(fn x ->
        # For each group, get the intersection (there should only be 1) and convert from ascii to priority
        getIntersection(x, Enum.at(x, 0))
          |> Enum.at(0)
          |> convert_ascii_to_priority
      end)
  end

  @doc "Get a list of groups - either two per line (:half) or every three (:three)"
  def getGroups(input, type) do
    rows = input
      |> String.trim
      |> String.split("\n")
      |> Enum.map(&String.to_charlist/1)

    case type do
      :half -> rows
        |> Enum.map(fn x -> x |> Enum.split(trunc(length(x)/2)) |> Tuple.to_list end)
      :three -> rows
        |> Enum.chunk_every(3)
    end
  end

  @doc "Recursive function to get the intersection of multiple lists"
  def getIntersection([head | tail], acc) do
    acc = MapSet.intersection(MapSet.new(head), MapSet.new(acc)) |> MapSet.to_list
    getIntersection(tail, acc)
  end
  def getIntersection([], acc), do: acc

  @doc """
    Convert to "priority" number based on ascii number
    Lowercase a is 97 and needs to subtract 96, Uppercase A is 65 and needs to subtract 38
  """
  def convert_ascii_to_priority(ascii) do
    if ascii >= 97, do: ascii - 96, else: ascii - 38
  end
end

{:ok, input} = File.read('2022/03/input.txt')
prioritiesPerRucksack = DayThree.getPriorities(input, :half)
IO.inspect(prioritiesPerRucksack |> Enum.sum, label: "Sum of priorities by rucksack")
prioritiesPerGroup = DayThree.getPriorities(input, :three)
IO.inspect(prioritiesPerGroup |> Enum.sum , label: "Sum of priorities by group")

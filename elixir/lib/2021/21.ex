import AOC

# https://adventofcode.com/2021/day/21
aoc 2021, 21 do
  def p1(input) do
    [p1, p2] = parse_input(input)
    emulate({p1, 0}, {p2, 0}, 0, 0)
  end

  def p2(input) do
    parse_input(input)
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line -> String.split(line, ": ") |> Enum.at(1) |> String.to_integer() end)
  end

  # Base case - p2score is >= 1000 - end round and calculate score
  def emulate({_, p1score}, {_, p2score}, _, num_rolls) when p2score >= 1000,
    do: p1score * num_rolls

  # Base case - same as above but opposite player
  def emulate({_, p1score}, {_, p2score}, _, num_rolls) when p1score >= 1000,
    do: p2score * num_rolls

  # Emulate a round, keeping track of the last roll value and the number of rolls completed so far
  def emulate({p1pos, p1score}, {p2pos, p2score}, last_roll, num_rolls) do
    # 3 rolls incrementing from the last roll, wrapping from 100 to 1
    rolls = for i <- 1..3, do: rem(last_roll + i - 1, 100) + 1
    last_roll = Enum.at(rolls, -1)
    # Next position is the sum of rolls, wrapping from 10 to 1
    p1pos = rem(p1pos + Enum.sum(rolls) - 1, 10) + 1
    # Switch positions of the players so p2 goes next
    emulate({p2pos, p2score}, {p1pos, p1score + p1pos}, last_roll, num_rolls + 3)
  end
end

import AOC

# https://adventofcode.com/2021/day/21
aoc 2021, 21 do
  def p1(input) do
    [pos_1, pos_2] = parse_input(input)
    emulate_p1({pos_1, 0}, {pos_2, 0}, 0, 0)
  end

  def p2(input) do
    [pos_1, pos_2] = parse_input(input)
    :ets.new(:cache, [:set, :named_table])
    emulate_p2({pos_1, 0}, {pos_2, 0}, 1) |> Tuple.to_list() |> Enum.max()
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line -> String.split(line, ": ") |> Enum.at(1) |> String.to_integer() end)
  end

  # Base case - p2score is >= 1000 - end round and calculate score
  def emulate_p1({_, score_1}, {_, score_2}, _, num_rolls) when score_2 >= 1000,
    do: score_1 * num_rolls

  # Base case - same as above but opposite player
  def emulate_p1({_, score_1}, {_, score_2}, _, num_rolls) when score_1 >= 1000,
    do: score_2 * num_rolls

  # Emulate a round, keeping track of the last roll value and the number of rolls completed so far
  def emulate_p1({pos_1, score_1}, {pos_2, score_2}, last_roll, num_rolls) do
    # 3 rolls incrementing from the last roll, wrapping from 100 to 1
    rolls = for i <- 1..3, do: rem(last_roll + i - 1, 100) + 1
    last_roll = Enum.at(rolls, -1)
    # Next position is the sum of rolls, wrapping from 10 to 1
    pos_1 = rem(pos_1 + Enum.sum(rolls) - 1, 10) + 1
    # Switch positions of the players so p2 goes next
    emulate_p1({pos_2, score_2}, {pos_1, score_1 + pos_1}, last_roll, num_rolls + 3)
  end

  # Base case - p2score is >= 21 - end round and indicate second player as winner
  def emulate_p2(_, {_, score_2}, _) when score_2 >= 21, do: {0, 1}

  # Base case - same as above but opposite player
  def emulate_p2({_, score_1}, _, _) when score_1 >= 21, do: {1, 0}

  def emulate_p2({pos_1, score_1}, {pos_2, score_2}, turn) do
    cache = :ets.lookup(:cache, {pos_1, score_1, pos_2, score_2, turn})

    if length(cache) > 0 do
      cache |> Enum.at(0) |> elem(1)
    else
      # The value of each possible result of three dice rolls, along with the chances (out of 27)
      # that the value will happen
      res =
        [{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}]
        |> Enum.reduce({0, 0}, fn {roll, prob}, {acc_1, acc_2} ->
          # For each roll value, emulate the current player's move
          {win_1, win_2} =
            if turn == 1 do
              pos_1 = rem(pos_1 + roll - 1, 10) + 1
              emulate_p2({pos_1, score_1 + pos_1}, {pos_2, score_2}, 2)
            else
              pos_2 = rem(pos_2 + roll - 1, 10) + 1
              emulate_p2({pos_1, score_1}, {pos_2, score_2 + pos_2}, 1)
            end

          # Accumulate the number of p1 and p2 wins, accounting for the probability
          # of this situation
          {acc_1 + win_1 * prob, acc_2 + win_2 * prob}
        end)

      :ets.insert(:cache, {{pos_1, score_1, pos_2, score_2, turn}, res})
      res
    end
  end
end

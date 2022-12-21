import AOC

# https://adventofcode.com/2022/day/20
aoc 2022, 20 do
  def p1(input) do
    nums = parse_input(input)
    mix(nums, 1) |> sum_grove_coordinates |> IO.inspect()
  end

  def p2(input) do
    nums = parse_input(input) |> Enum.map(&Kernel.*(&1, 811_589_153))
    mix(nums, 10) |> sum_grove_coordinates |> IO.inspect()
  end

  @doc "Parse the input into a list of integers"
  def parse_input(input) do
    input |> String.trim() |> String.split("\n") |> Enum.map(&String.to_integer/1)
  end

  @doc "Mix the list of numbers 'times' times"
  def mix(nums, times) do
    length = length(nums)

    # Create a list of positions of the original numbers. To start, should just be [0, 1, 2, 3, 4, etc].
    # This list of positions will be modified rather than actually moving numbers around.
    positions = 0..(length - 1)

    new_positions =
      # Repeat the mix `times` times
      Enum.reduce(1..times, positions, fn _, indices ->
        nums
        |> Enum.with_index()
        # Iterate through each number in the original list, modifying the list of positions as we go
        |> Enum.reduce(indices, fn {num, num_idx}, acc ->
          # If we're on the 4th number, the 4th number in the positions list holds its position
          old_position = Enum.at(acc, num_idx)

          # Determine the new "looped" position
          new_position =
            case old_position + num do
              # If we're over the end of the list, mod to get looped position.
              # Mod by "length - 1" because position before first number and last number are the same
              new_position when new_position > length -> rem(new_position, length - 1)
              # If negative index, mod and add length to get looped position
              new_position when new_position <= 0 -> rem(new_position, length - 1) + (length - 1)
              # Otherwise, keep index as is
              new_position -> new_position
            end

          # Loop through all positions
          acc
          |> Enum.with_index()
          |> Enum.map(fn {position, position_idx} ->
            cond do
              # If this position is the number we moved, set it to the new index
              position_idx == num_idx -> new_position
              # If we moved the number forward in the list, decrement all the numbers between the two positions
              new_position > old_position && position > old_position && position <= new_position -> position - 1
              # If we moved backwards, increment all numbers between the two positions
              new_position < old_position && position < old_position && position >= new_position -> position + 1
              true -> position
            end
          end)
        end)
      end)

    # Use the positions to arrange the new numbers
    arrange_nums(nums, new_positions)
  end

  @doc "Arrange the list of numbers based on the given indices"
  def arrange_nums(nums, indices) do
    indices
    |> Enum.with_index()
    |> Enum.reduce(nums, fn {new_index, num_idx}, acc ->
      List.replace_at(acc, new_index, Enum.at(nums, num_idx))
    end)
  end

  @doc "Get the sum of grove coordinates - sum 1000th, 2000th, 3000th numbers after the number 0"
  def sum_grove_coordinates(nums) do
    length = length(nums)
    zero_idx = Enum.find_index(nums, fn x -> x == 0 end)

    [1000, 2000, 3000]
    |> Enum.map(fn x -> Enum.at(nums, rem(zero_idx + x, length)) end)
    |> Enum.sum()
  end
end

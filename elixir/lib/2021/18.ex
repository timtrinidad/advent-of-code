import AOC

# https://adventofcode.com/2021/day/18
aoc 2021, 18 do
  def p1(input) do
    [first | all] = parse_input(input)
    Enum.reduce(all, first, fn list, acc -> add(acc, list) end) |> magnitude
  end

  def p2(input) do
    parse_input(input)
  end

  @doc "Convert each line into a list of tuples {val, depth}"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.reduce({0, []}, fn char, {depth, list} ->
        is_char_num = Regex.match?(~r/^[0-9]$/, char)

        case char do
          "[" -> {depth + 1, list}
          "]" -> {depth - 1, list}
          _ when is_char_num -> {depth, list ++ [{String.to_integer(char), depth}]}
          _ -> {depth, list}
        end
      end)
      |> elem(1)
    end)
  end

  @doc "Add two lists together - concatenate the list of tuples increase each depth by 1. Reduce."
  def add(list1, list2) do
    (list1 ++ list2) |> Enum.map(fn {val, depth} -> {val, depth + 1} end) |> reduce
  end

  @doc "Recursively reduce until it can no longer be exploded or split"
  def reduce(list) do
    cond do
      (idx = explode_idx(list)) != nil -> list |> explode(idx) |> reduce
      (idx = split_idx(list)) != nil -> list |> split(idx) |> reduce
      true -> list
    end
  end

  @doc "Determine the 2 indices of the values which should be exploded. nil if none."
  def explode_idx(list) do
    res =
      list
      |> Enum.with_index()
      |> Enum.reduce_while(nil, fn {{_, depth}, idx}, last_depth ->
        cond do
          depth >= 5 && depth == last_depth -> {:halt, {idx - 1, idx}}
          true -> {:cont, depth}
        end
      end)

    # If we didn't receive a tuple, that means no explode
    # candidates were found. Return nil.
    if is_tuple(res), do: res, else: nil
  end

  @doc "Explode the numbers at the given indices "
  def explode(list, {idx_l, idx_r}) do
    {val_l, _} = Enum.at(list, idx_l)
    {val_r, _} = Enum.at(list, idx_r)
    # Prepend and reverse rather than append for speed improvement
    list
    |> Enum.with_index()
    |> Enum.reduce([], fn {{curr_val, curr_depth}, idx}, acc ->
      cond do
        # For the value immediately to the left of the left exploded number, increment by that number
        idx == idx_l - 1 -> [{val_l + curr_val, curr_depth} | acc]
        # Same for the right
        idx == idx_r + 1 -> [{val_r + curr_val, curr_depth} | acc]
        # Replace the item at left index with 0
        idx == idx_l -> [{0, curr_depth - 1} | acc]
        # Don't do anything for the item at the right index
        idx == idx_r -> acc
        # Copy remaining numbers as is
        true -> [{curr_val, curr_depth} | acc]
      end
    end)
    |> Enum.reverse()
  end

  @doc "Determine the index of the number which should be split. nil if none."
  def split_idx(list), do: Enum.find_index(list, fn {num, _} -> num >= 10 end)

  @doc "Split the number at the given index."
  def split(list, idx_split) do
    {val, _} = Enum.at(list, idx_split)

    # Half rounded down
    new_val_l = div(val, 2)
    # Half rounded up
    new_val_r = if rem(val, 2) == 0, do: new_val_l, else: new_val_l + 1

    # Prepend and reverse rather than append for speed improvement
    list
    |> Enum.with_index()
    |> Enum.reduce([], fn {{curr_val, curr_depth}, idx}, acc ->
      cond do
        # At the position of the original number, add a new number pair (1 depth more than the original number)
        idx == idx_split -> [{new_val_r, curr_depth + 1}, {new_val_l, curr_depth + 1} | acc]
        # Copy all other numbers as is.
        true -> [{curr_val, curr_depth} | acc]
      end
    end)
    |> Enum.reverse()
  end

  @doc "Calculate the magnitude of a list of {num, depth} tuples"
  # Base case - one single value at the top level
  def magnitude([{val, 0}]), do: val

  @doc "Recursively calculate the magnitude of the deepest, left-most number pair."
  def magnitude(list) do
    deepest = list |> Enum.map(&elem(&1, 1)) |> Enum.max()

    # Find the index of the numbers which should be calculated/replaced
    {idx_l, idx_r} =
      list
      |> Enum.with_index()
      |> Enum.reduce_while(nil, fn {{_, depth}, idx}, last_depth ->
        cond do
          depth == deepest && depth == last_depth -> {:halt, {idx - 1, idx}}
          true -> {:cont, depth}
        end
      end)

    {val_l, _} = Enum.at(list, idx_l)
    {val_r, _} = Enum.at(list, idx_r)

    # Copy the list, replacing the two numbers with a single magnitude number at one less depth
    list
    |> Enum.with_index()
    |> Enum.reduce([], fn {{num, depth}, idx}, acc ->
      cond do
        idx == idx_l -> [{3 * val_l + 2 * val_r, depth - 1} | acc]
        idx == idx_r -> acc
        true -> [{num, depth} | acc]
      end
    end)
    |> Enum.reverse()
    |> magnitude
  end
end

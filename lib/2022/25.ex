import AOC

# https://adventofcode.com/2022/day/25
aoc 2022, 25 do
  def p1(input) do
    sum = parse_input(input) |> Enum.map(&snafu_string_to_dec/1) |> Enum.sum() |> IO.inspect()
    dec_to_snafu_array(sum) |> snafu_array_to_string |> IO.inspect()
  end

  def p2(_input) do
  end

  @doc "Basic parsing of input into a list of lines"
  def parse_input(input) do
    input |> String.trim() |> String.split("\n")
  end

  @doc "Convert a snafu string to a decimal number"
  def snafu_string_to_dec(snafu) do
    # First convert a string like "2=-1=2" into list [2, -2, -1, 1, -2, 2]
    snafu
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.map(fn x ->
      case x do
        "-" -> -1
        "=" -> -2
        x -> String.to_integer(x)
      end
    end)
    # Then convert that list to a decimal number
    |> snafu_to_dec
  end

  @doc "Convert a snafu array to a decimal number"
  def snafu_to_dec(snafu) do
    snafu
    |> Enum.with_index()
    |> Enum.reduce(0, fn {x, idx}, acc ->
      place = :math.pow(5, idx)
      trunc(acc + place * x)
    end)
  end

  @doc "Convert a decimal number into a snafu array"
  def dec_to_snafu_array(dec) do
    # 1st digit: 2
    # 2nd digit: 1: 3 to 7, 2: 8 to 12 (range 5)
    # 3rd digit: 1: 13 to 37, 2: 38 to 62 (range 25)
    # 4th digit: 1: 63 to 187, 2: 188 to 312

    {largest_place, largest_place_digit} =
      1..100
      |> Enum.reduce_while({0, 0}, fn x, acc ->
        pm_range = trunc(Enum.reduce(0..(x - 1), 0, fn x, acc -> acc + :math.pow(5, x) * 2 end))
        pow = trunc(:math.pow(5, x))

        cond do
          dec in (-2 * pow - pm_range)..(-2 * pow + pm_range) -> {:halt, {x, -2}}
          dec in (-1 * pow - pm_range)..(-1 * pow + pm_range) -> {:halt, {x, -1}}
          dec in (0 * pow - pm_range)..(0 * pow + pm_range) -> {:halt, {x, 0}}
          dec in (1 * pow - pm_range)..(1 * pow + pm_range) -> {:halt, {x, 1}}
          dec in (2 * pow - pm_range)..(2 * pow + pm_range) -> {:halt, {x, 2}}
          true -> {:cont, acc}
        end
      end)

    val = trunc(:math.pow(5, largest_place) * largest_place_digit)

    if largest_place == 1,
      do: [{1, largest_place_digit}, {0, dec - val}],
      else: [{largest_place, largest_place_digit}] ++ dec_to_snafu_array(dec - val)
  end

  @doc "Convert a snafu array into a snafu string"
  def snafu_array_to_string(input) do
    max_place = input |> Enum.map(&elem(&1, 0)) |> Enum.max()

    # Fill in missing places with 0
    max_place..0
    |> Enum.map(fn place ->
      {_, value} = Enum.find(input, {place, 0}, fn {x, _} -> x == place end)
      value
    end)
    |> Enum.map(fn x ->
      case x do
        -2 -> "="
        -1 -> "-"
        x -> Integer.to_string(x)
      end
    end)
    |> Enum.join("")
  end
end

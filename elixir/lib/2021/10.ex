# https://adventofcode.com/2021/day/10
import AOC

aoc 2021, 10 do
  @char_pairs %{"<" => ">", "[" => "]", "(" => ")", "{" => "}"}
  @char_values %{")" => 3, "]" => 57, "}" => 1197, ">" => 25137}

  def p1(input) do
    parse_input(input)
    |> Enum.map(&get_first_illegal_char/1)
    |> Enum.map(&Map.get(@char_values, &1, 0))
    |> Enum.sum()
  end

  def p2(input) do
    parse_input(input)
  end

  @doc "Parse input into a list of characters by line"
  def parse_input(input) do
    input |> String.trim() |> String.split("\n") |> Enum.map(&String.graphemes/1)
  end

  @doc "Keep track of the stack of open characters and return the first illegal character, or 'nil' if the line is correct or incomplete."
  def get_first_illegal_char(line) do
    illegal_char =
      line
      |> Enum.reduce_while([], fn char, acc ->
        cond do
          # Is opening char
          Map.has_key?(@char_pairs, char) -> {:cont, [char | acc]}
          # Is closing char for the head of the stack
          @char_pairs[hd(acc)] == char -> {:cont, tl(acc)}
          # Otherwise, illegal char
          true -> {:halt, char}
        end
      end)

    # If we have a list, the reduce finished - we have incomplete line and no illegal charachter was encountered
    if is_list(illegal_char), do: nil, else: illegal_char
  end
end

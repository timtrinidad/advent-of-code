# https://adventofcode.com/2021/day/10
import AOC

aoc 2021, 10 do
  @char_pairs %{"<" => ">", "[" => "]", "(" => ")", "{" => "}"}
  @char_values_pt1 %{")" => 3, "]" => 57, "}" => 1197, ">" => 25137}
  @char_values_pt2 %{"(" => 1, "[" => 2, "{" => 3, "<" => 4}

  def p1(input) do
    parse_input(input)
    |> Enum.map(&analyze_line/1)
    |> Enum.map(fn
      # For all corrupt lines, get the value of the offending character
      {:corrupt, char} -> @char_values_pt1[char]
      # Otherwise, line has a score of 0
      {_, _} -> 0
    end)
    |> Enum.sum()
  end

  def p2(input) do
    parse_input(input)
    |> Enum.map(&analyze_line/1)
    # Filter for only incomplete lines
    |> Enum.filter(&(elem(&1, 0) == :incomplete))
    |> Enum.map(fn {_, stack} ->
      # For each unclosed character, multiply existing score by 5 and add value of current char
      stack |> Enum.reduce(0, fn char, acc -> 5 * acc + @char_values_pt2[char] end)
    end)
    |> Math.Enum.median()
  end

  @doc "Parse input into a list of characters by line"
  def parse_input(input) do
    input |> String.trim() |> String.split("\n") |> Enum.map(&String.graphemes/1)
  end

  @doc "Analyze a line and return a tuple with status of either :complete, :incomplete, or :corrupt"
  def analyze_line(line) do
    illegal_char =
      line
      # Keep track of stack of open characters which haven't been closed
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

    cond do
      # Corrupt line - line has illegal character
      !is_list(illegal_char) -> {:corrupt, illegal_char}
      # Complete/correct line
      length(illegal_char) == 0 -> {:complete, nil}
      # Incomplete line - return stack
      true -> {:incomplete, illegal_char}
    end
  end
end

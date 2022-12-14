# https://adventofcode.com/2022/day/6

defmodule DaySix do
  def exec(input, num_unique_chars) do
    input
      |> String.trim
      |> String.to_charlist
      |> Enum.with_index(1)
      # Loop through, maintaining a list of num_unique_chars last characters.
      |> Enum.reduce_while([], fn {x, idx}, acc ->
        # If the list already has more characters than we want, pop off the first element.
        acc = (if length(acc) < num_unique_chars, do: acc, else: tl(acc)) ++ [x]
        cond do
          length(acc) < num_unique_chars -> {:cont, acc}
          length(acc) != length(Enum.uniq(acc)) -> {:cont, acc}
          true -> {:halt, idx}
        end
      end)
  end
end

{:ok, input} = File.read('2022/06/input.txt')

for i <- [4, 14] do
  DaySix.exec(input, i)
    |> IO.inspect(label: "# chars before seeing #{i} unique characters")
end

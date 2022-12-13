# https://adventofcode.com/2022/day/13

defmodule DayThirteen do
  @doc "Return the sum of correct indices and decoder key"
  def exec(input) do
    packet_pairs = parse_input(input)

    # For each pair, run comparison function and sum correctly sorted indices
    correct_indices_sum = packet_pairs
      |> Enum.with_index
      |> Enum.map(fn {x, idx} ->
        is_correct = compare(x)
        if is_correct == 1, do: idx+1, else: 0
      end)
      |> Enum.sum

    # Flatten pairs into single list. Run comparison function to sort.
    decoder_key_packets = [[[2]], [[6]]]
    decoder_key = packet_pairs
      |> Enum.concat()
      # Inject decoder key packets
      |> Kernel.++(decoder_key_packets)
      |> Enum.sort(fn a, b -> compare([a, b]) == 1 end)
      |> Enum.with_index
      # Multiply the indices of the decoder packets
      |> Enum.reduce(1, fn {x, idx}, acc ->
        if Enum.member?(decoder_key_packets, x), do: (acc * (idx + 1)), else: acc
      end)

    {correct_indices_sum, decoder_key}
  end

  @doc "Parse the input into pairs of lists"
  def parse_input(input) do
    input
    |> String.trim
    |> String.split("\n\n")
    |> Enum.map(fn x ->
      String.split(x, "\n")
      # Some protection just in case
      |> Enum.filter(&String.match?(&1, ~r/^[\[\],\d]+$/))
      |> Enum.map(fn y -> {result, _} = Code.eval_string(y); result end)
    end)
  end

  @doc "Compare based on challenge rules"
  def compare([[l | l_rest], [r | r_rest]]) do
    cond do
      # If both lists. If both are equal, move onto rest of list. Otherwise, stop comparison.
      is_list(l) and is_list(r) -> curr = compare([l, r]); if curr == 0, do: compare([l_rest, r_rest]), else: curr
      # If either left or right is not a list, wrap in list and try again
      is_list(l) and is_integer(r) -> compare([[l] ++ l_rest, [[r]] ++ r_rest])
      is_integer(l) and is_list(r) -> compare([[[l]] ++ l_rest, [r] ++ r_rest])
      # If both equal integers, move onto the rest of the list
      l == r -> compare([l_rest, r_rest])
      # If both integers, return correct/incorrect and stop comparing.
      l > r -> -1
      r > l -> 1
    end
  end
  # Both empty arrays - continue onto rest of list
  def compare([[], []]), do: 0
  # If either left or right is an empty array, return correct/incorrect and stop comparing
  def compare([[], _]), do: 1
  def compare([_, []]), do: -1

end

{:ok, input} = File.read('2022/13/input.txt')

{correct_indices_sum, decoder_key} = DayThirteen.exec(input)
IO.inspect(correct_indices_sum, label: "Sum of indices of correctly ordered pairs")
IO.inspect(decoder_key, label: "Decoder key")
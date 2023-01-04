import AOC

# https://adventofcode.com/2021/day/16
aoc 2021, 16 do
  def p1(input) do
    {ver, val, _} = parse_input(input) |> parse_packet
    sum_versions({ver, val})
  end

  def p2(input) do
    {ver, val, _} = parse_input(input) |> parse_packet
    reduce_val({ver, val})
  end

  @doc "Parse input string into a bitstring"
  def parse_input(input) do
    input |> :binary.decode_hex()
  end

  # Version is first three bits, second three bits is "4" (literal), and "rest" for remainder of bits
  def parse_packet(<<version::3, 4::3, rest::bitstring>>) do
    # Parse the literal value and return remaining unused bits
    {literal, rest} = parse_literal(rest)
    {version, bits_to_int(literal), rest}
  end

  # Version is first three bits, second three bits designate the operator, and "rest" for remainder of bits
  def parse_packet(<<version::3, operator::3, rest::bitstring>>) do
    func =
      case operator do
        0 -> &Enum.sum/1
        1 -> &Enum.product/1
        2 -> &Enum.min/1
        3 -> &Enum.max/1
        5 -> &if Enum.at(&1, 0) > Enum.at(&1, 1), do: 1, else: 0
        6 -> &if Enum.at(&1, 0) < Enum.at(&1, 1), do: 1, else: 0
        7 -> &if Enum.at(&1, 0) == Enum.at(&1, 1), do: 1, else: 0
      end

    parse_operator(version, rest, func)
  end

  # Parse the operands and store the proper operator function
  def parse_operator(version, bits, func) do
    {val, rest} = parse_operand(bits)
    {version, {func, val}, rest}
  end

  # Base case for type length - no more bits to process.
  def parse_operand(<<0::1, 0::15, rest::bitstring>>), do: {[], rest}

  # First bit is 0 (type length), next 15 bits is the length of the packets, and remainder of bits in "rest"
  def parse_operand(<<0::1, length_packets::15, rest::bitstring>>) do
    orig_bit_size = bit_size(rest)
    {version, val, rest} = parse_packet(rest)
    processed_bit_size = orig_bit_size - bit_size(rest)

    {acc, rest} =
      parse_operand(<<0::1, length_packets - processed_bit_size::15, rest::bitstring>>)

    {[{version, val}] ++ acc, rest}
  end

  # Base case - type "num" and no more packets to process - return accumulated values
  def parse_operand(<<1::1, 0::11, rest::bitstring>>), do: {[], rest}
  # First bit is 1 (type num), next 11 bits is num packets, remainer is in "rest"
  def parse_operand(<<1::1, num_packets::11, rest::bitstring>>) do
    # Parse the set of packets until the remaining number of packets is exhausted
    {version, val, rest} = parse_packet(rest)
    {acc, rest} = parse_operand(<<1::1, num_packets - 1::11, rest::bitstring>>)
    {[{version, val}] ++ acc, rest}
  end

  # Literal - first bit is 0 (this is the last set of 5), next 4 bits indicates a value
  def parse_literal(<<0::1, val::4, rest::bitstring>>), do: {<<val::4>>, rest}

  # Literal - first bit is 1 (more sets of 5 to process), next 4 bits indicates value, process "rest"
  def parse_literal(<<1::1, val::4, rest::bitstring>>) do
    {next_val, rest} = parse_literal(rest)
    {<<(<<val::4>>), next_val::bitstring>>, rest}
  end

  @doc "Convert a literal value as bits to an integer"
  def bits_to_int(bits) do
    size = bit_size(bits)
    <<n::size(size)>> = bits
    n
  end

  @doc "Recursively find and sum all version numbers"
  def sum_versions({ver, {_, val}}) when is_list(val),
    do: ver + Enum.sum(Enum.map(val, &sum_versions/1))

  # Base case - val is not a list, no more recursion. Simply return version.
  def sum_versions({ver, _}), do: ver

  def reduce_val({_, {func, val}}) when is_list(val), do: func.(Enum.map(val, &reduce_val/1))
  # Base case - val is not a list, no more recursion. Simply return value.
  def reduce_val({_, val}), do: val
end

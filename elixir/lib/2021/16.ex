import AOC

# https://adventofcode.com/2021/day/16
aoc 2021, 16 do
  def p1(input) do
    bits = parse_input(input)
    {ver, val, _} = parse_packet(bits) |> IO.inspect()
    sum_versions({ver, val})
  end

  def p2(input) do
    parse_input(input)
  end

  def parse_input(input) do
    input |> :binary.decode_hex()
  end

  # Version is first three bits, second three bits is "4" (literal), and "rest" for remainder of bits
  def parse_packet(<<version::3, 4::3, rest::bitstring>>) do
    # Parse the literal value and return remaining unused bits
    {literal, rest} = parse_literal(rest)
    {version, bits_to_int(literal), rest}
  end

  # Version is first three bits, second three bits is not 4 (discard), next bit is 0 (type length)
  # next 15 bits is the length of the packets, and remainder of bits in "rest"
  def parse_packet(<<version::3, _::3, 0::1, length_packets::15, rest::bitstring>>) do
    # Parse the set of packets until remaining length is exhausted
    {val, rest} = parse_package(:length, length_packets, rest, [])
    {version, val, rest}
  end

  # Version is first three bits, second three bits is not 4 (discard), next bit is 1 (type num)
  # next 11 bits is num packets, remainer is in "rest"
  def parse_packet(<<version::3, _::3, 1::1, num_packets::11, rest::bitstring>>) do
    # Parse the set of packets until the remaining number of packets is exhausted
    {val, rest} = parse_package(:num, num_packets, rest, [])
    {version, val, rest}
  end

  # Base case - type "num" and no more packets to process - return accumulated values
  def parse_package(:num, 0, rest, acc), do: {acc, rest}

  # Parse the bits as packets until num_packages is exhausted
  def parse_package(:num, num, bits, acc) do
    {version, val, rest} = parse_packet(bits)
    parse_package(:num, num - 1, rest, acc ++ [{version, val}])
  end

  # Base case - type "length" and no more bits to process - return accumulated values
  def parse_package(:length, 0, rest, acc), do: {acc, rest}

  # Parse the bits as packets until num_bits is exhausted
  def parse_package(:length, length, bits, acc) do
    orig_bit_size = bit_size(bits)
    {version, val, rest} = parse_packet(bits)
    processed_bit_size = orig_bit_size - bit_size(rest)
    parse_package(:length, length - processed_bit_size, rest, acc ++ [{version, val}])
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
  def sum_versions({ver, val}) when is_list(val),
    do: ver + Enum.sum(Enum.map(val, &sum_versions/1))

  # Base case - val is not a list, no more recursion. Simply return version.
  def sum_versions({ver, _}), do: ver
end

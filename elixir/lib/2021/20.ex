import AOC

# https://adventofcode.com/2021/day/20
aoc 2021, 20 do
  def p1(input) do
    {image, true} = parse_input(input) |> process(true, 2)
    image |> MapSet.size()
  end

  def p2(input) do
    {image, true} = parse_input(input) |> process(true, 50)
    image |> MapSet.size()
  end

  @doc "Parse the input into a mapset of lit coordinates in the 'image' and a mapset of lit values in 'enhancement'"
  def parse_input(input) do
    [enhancement, image] = input |> String.trim() |> String.split("\n\n")

    enhancement =
      enhancement
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn {char, idx}, acc ->
        if char == ?#, do: MapSet.put(acc, idx), else: acc
      end)

    image =
      image
      |> String.split("\n")
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn {line, y}, acc ->
        line
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {char, x}, acc ->
          if char == ?#, do: MapSet.put(acc, {x, y}), else: acc
        end)
      end)

    {enhancement, image}
  end

  @doc "Recursively expand the image for the given number of rounds"
  # Base case - no more rounds left
  def process({_, image}, image_is_light, 0), do: {image, image_is_light}

  def process({enhancement, image}, image_is_light, num_times) do
    # If the first bit is 0 and last is 1, then the infinite space flips every other turn.
    # The image should alternately keep track of only lit bits one turn and unlit bits the next
    next_image_is_light =
      if MapSet.member?(enhancement, 0), do: !image_is_light, else: image_is_light

    {min_x, max_x} = image |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
    {min_y, max_y} = image |> Enum.map(&elem(&1, 1)) |> Enum.min_max()

    coords = for x <- (min_x - 3)..(max_x + 3), y <- (min_y - 3)..(max_y + 3), do: {x, y}

    # Go through all image coordinates to determine what it should be changed to into the next image.
    image =
      coords
      |> Enum.reduce(MapSet.new(), fn {x, y}, acc ->
        # For each of the bits, determine if the cell in the neighboring cell is lit.
        # Convert into a bitstring and extract the number.
        <<val::9>> =
          [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {0, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}]
          |> Enum.reduce(<<>>, fn {dx, dy}, acc ->
            is_light = image_is_light == MapSet.member?(image, {x + dx, y + dy})
            <<acc::bitstring, if(is_light, do: 1, else: 0)::1>>
          end)

        if MapSet.member?(enhancement, val) == next_image_is_light,
          do: MapSet.put(acc, {x, y}),
          else: acc
      end)

    process({enhancement, image}, next_image_is_light, num_times - 1)
  end
end

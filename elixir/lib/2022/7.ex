import AOC

# https://adventofcode.com/2022/day/7
aoc 2022, 7 do
  def p1(input) do
    input |> parse_input |> dir_sizes([], %{}) |> Map.values |> Enum.filter(&(&1 <= 100000)) |> Enum.sum
  end

  def p2(input) do
    sizes = input |> parse_input |> dir_sizes([], %{})
    space_needed = 30000000 - (70000000 - sizes["/"])
    sizes |> Map.values |> Enum.filter(&(&1 > space_needed)) |> Enum.sort |> hd
  end

  def parse_input(input) do
    input |> String.trim |> String.split("\n") |> Enum.map(fn line -> line |> String.replace("$ ", "") |> String.split(" ") end)
  end

  def dir_sizes([], _, sizes), do: sizes
  def dir_sizes([["cd", "/"] | rest], cwd, sizes), do: dir_sizes(rest, ["/"], sizes)
  def dir_sizes([["cd", ".."] | rest], cwd, sizes), do: dir_sizes(rest, tl(cwd), sizes)
  def dir_sizes([["cd", path] | rest], cwd, sizes), do: dir_sizes(rest, [path | cwd], sizes)
  def dir_sizes([[cmd | _] | rest], cwd, sizes) when cmd in ["ls", "dir"], do: dir_sizes(rest, cwd, sizes)
  def dir_sizes([[filesize | _] | rest], cwd, sizes) do
    sizes = Enum.reduce(1..length(cwd), sizes, fn part, acc ->
      path = Enum.take(cwd, -part) |> Enum.join("/")
      filesize = String.to_integer(filesize)
      Map.update(acc, path, filesize, &(&1 + filesize))
    end)
    dir_sizes(rest, cwd, sizes)
  end
end

import AOC

# https://adventofcode.com/2022/day/7
aoc 2022, 7 do
  def p1(input) do
    # Find sum of directories less than 100k bytes
    input
    |> parse_input
    |> dir_sizes([], %{})
    |> Map.values()
    |> Enum.filter(&(&1 <= 100_000))
    |> Enum.sum()
  end

  def p2(input) do
    # Find smallest directory to delete
    sizes = input |> parse_input |> dir_sizes([], %{})
    space_needed = 30_000_000 - (70_000_000 - sizes["/"])
    sizes |> Map.values() |> Enum.filter(&(&1 > space_needed)) |> Enum.sort() |> hd
  end

  @doc "Parse input into list of commands, discarding anything but filesizes and cd commands"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line -> line |> String.replace("$ ", "") |> String.split(" ") end)
    |> Enum.filter(&(Enum.at(&1, 0) not in ["ls", "dir"]))
  end

  @doc "Recursively process commands to get size of directories"
  # Base case - no more commands
  def dir_sizes([], _, sizes), do: sizes

  # cd commands
  def dir_sizes([["cd", path] | cmds], cwd, sizes) do
    cwd =
      case path do
        "/" -> ["/"]
        ".." -> tl(cwd)
        path -> [path | cwd]
      end

    dir_sizes(cmds, cwd, sizes)
  end

  # Filesize commands
  def dir_sizes([[filesize, _] | cmds], cwd, sizes) do
    sizes =
      # Updat the filesize of the dir and parent dirs
      Enum.reduce(1..length(cwd), sizes, fn part, acc ->
        path = Enum.take(cwd, -part) |> Enum.join("/")
        filesize = String.to_integer(filesize)
        Map.update(acc, path, filesize, &(&1 + filesize))
      end)

    dir_sizes(cmds, cwd, sizes)
  end
end

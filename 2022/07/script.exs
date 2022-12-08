# https://adventofcode.com/2022/day/7

defmodule DaySeven do
  @doc "Process each line of the input using recursion"
  def process_list([head | tail], sizes, path) do
    {sizes, path} = handle_line(head, sizes, path)
    process_list(tail, sizes, path)
  end

  # "Base case of recursion - no lines left to process"
  def process_list([], sizes, path) do
    {sizes, path}
  end

  @doc "Process a single line"
  def handle_line(line, sizes, path) do
    cond do
      # Line starts with "$" - treat this as a command
      String.at(line, 0) == "$" ->
        # Split by " " and remove starting "$"
        command = String.split(line, " ") |> List.delete_at(0)
        IO.puts("#{path |> Enum.join("/")}$ #{command |> Enum.join(" ")}")
        handle_command(command, sizes, path)
      # Output
      true ->
        # Split by " "
        output = String.split(line, " ")
        IO.puts("#{path |> Enum.join("/")}: #{line}")
        handle_output(output, sizes, path)
    end
  end

  @doc "Handle the 'cd' command and adjust the path list as needed"
  def handle_command(["cd" | args], sizes, path) do
    cd_dir = Enum.at(args, 0)
    path = case cd_dir do
      # Reset to base path
      "/" -> []
      # Pop the end of the path list
      ".." -> {_, path2} = List.pop_at(path, -1); path2
      # Append to the end of the path list
      x -> path ++ [x]
    end
    {sizes, path}
  end

  # "Handle the 'ls' command"
  def handle_command(["ls" | _], sizes, path) do
    # Do nothing
    {sizes, path}
  end

  @doc "Handle the output from a command (not staring with '$')"
  def handle_output([first_arg, second_arg], sizes, path) do
    # Only handle responses from 'ls' which start with a number
    # Ignore base path
    if is_numeric(first_arg)  do
      # Parse first number as int and prepend "/"
      {size, _} = Integer.parse(first_arg)
      size_path = "/" <> (path |> Enum.join("/"))
      # Increment size of path
      existing_size = (sizes[size_path] || 0)
      sizes = put_in(sizes, [size_path], existing_size + size)
      # Using recursion, also increment parent paths
      {sizes, _} = if length(path) > 0 do
        {_, parent_path} = List.pop_at(path, -1)
        handle_output([first_arg, second_arg], sizes, parent_path)
      else
        {sizes, path}
      end
      {sizes, path}
    else
      {sizes, path}
    end
  end

  # "Ignore all other outputs"
  def handle_output(_, sizes, path) do
    # do nothing
    {sizes, path}
  end

  # From https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Elixir
  def is_numeric(str) do
    case Float.parse(str) do
      {_num, ""} -> true
      _          -> false
    end
  end
end

# Read input and split by line
{:ok, input} = File.read('2022/07/input.txt')
lines = String.split(input, "\n")
# Get full directory paths and their sizes
{sizes, _} = DaySeven.process_list(lines, %{}, [])

# Filter for directories with less than 100,000 bytes and sum
filtered_total = Map.values(sizes) |> Enum.filter(fn x -> x < 100000 end) |> Enum.sum()
IO.puts("Sum of small folder sizes: #{filtered_total}")
used_space = sizes["/"]
IO.puts("Used space: #{used_space}")
remaining_space = 70000000 - used_space
IO.puts("Remaining space: #{remaining_space}")
needed_additional_space = 30000000 - remaining_space
IO.puts("Needed additional space: #{needed_additional_space}")
smallest_valid_size = Map.values(sizes) |> Enum.filter(fn x -> x > needed_additional_space end) |> Enum.min()
IO.puts("Smallest directory to delete: #{smallest_valid_size}")


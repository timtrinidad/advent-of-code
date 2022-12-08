# https://adventofcode.com/2022/day/7

defmodule DaySeven do
  @doc "Get folder sizes based on the saved shell output"
  def get_folder_sizes(input) do
    lines = String.split(input, "\n")
    {sizes, _} = process_list(lines, %{}, [])
    sizes
  end

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
     # Handle directory changes
     (match = Regex.run(~r/^\$ cd (.+)/, line)) ->
       [_, dir] = match
       path = case dir do
         "/" -> [] # cd /
         ".." -> List.delete_at(path, -1) # cd ..
         x -> path ++ [x] # cd [folder]
       end
       {sizes, path}
     # Handle filesize responses
     (match = Regex.run(~r/^(\d+) .+/, line)) ->
        [_, filesize] = match
        filesize = String.to_integer(filesize)
        full_path = "/" <> Enum.join(path, "/")
        sizes = put_in(sizes, [full_path], (sizes[full_path] || 0) + filesize)
        {sizes, _} = if length(path) > 0 do
          # Save folder sizes for parent folders
          handle_line(line, sizes, List.delete_at(path, -1))
        else
          {sizes, path}
        end
        {sizes, path}
      true -> {sizes, path}
    end
  end
end

# Read input and split by line
{:ok, input} = File.read('2022/07/input.txt')
sizes = DaySeven.get_folder_sizes(input)

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


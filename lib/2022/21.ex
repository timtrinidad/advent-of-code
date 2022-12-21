import AOC

# https://adventofcode.com/2022/day/21
aoc 2022, 21 do
  def p1(input) do
    jobs = parse_input(input)
    get_value(jobs, "root") |> IO.inspect
  end

  def p2(input) do
  end

  @doc "Parse the input into a Map of jobs"
  def parse_input(input) do
    input |> String.trim |> String.split("\n") |> Enum.map(fn line ->
      [key, val] = line |> String.split(": ")
      if !String.contains?(val, " ") do
        {key, %{type: "val", val: String.to_integer(val)}}
      else
        [op1, op, op2] = String.split(val, " ")
        op = case op do
          "+" -> &Kernel.+/2
          "-" -> &Kernel.-/2
          "*" -> &Kernel.*/2
          "/" -> &Kernel.//2
        end
        {key, %{type: "op", op1: op1, op2: op2, op: op}}
      end
    end) |> Enum.into(%{})
  end

  @doc "Recursively calculate value based on job"
  def get_value(jobs, name) do
    job = jobs[name]
    cond do
      job.type == "val" -> job.val
      true -> job.op.(get_value(jobs, job.op1), get_value(jobs, job.op2))
    end
  end
end

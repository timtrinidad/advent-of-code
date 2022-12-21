import AOC

# https://adventofcode.com/2022/day/21
aoc 2022, 21 do
  def p1(input) do
    jobs = parse_input(input)
    get_value(jobs, "root") |> IO.inspect
  end

  def p2(input) do
    jobs = parse_input(input) |> IO.inspect()

    # Build a dependency graph so we know the path from "root" to "humn"
    graph = Graph.new()
    graph = jobs |> Enum.reduce(graph, fn {key, _}, acc ->
      Graph.add_vertex(acc, key)
    end)
    graph = jobs |> Enum.reduce(graph, fn {key, job}, acc ->
      if job.type == "op", do: Graph.add_edges(acc, [{key, job.op1}, {key, job.op2}]), else: acc
    end)
    path_to_human = Graph.get_shortest_path(graph, "root", "humn")
    [_, human_side | remaining_human_path] = path_to_human

    # Determine the non-human side of the root equation
    root_job = jobs["root"]
    other_side = if root_job.op1 == human_side, do: root_job.op2, else: root_job.op1
    other_value = get_value(jobs, other_side)

    # We know the human side of the root equation is equal to other_value.
    # Go down the path and infer the value of the non-human operand for each elment of the path.
    infer_operand(jobs, other_value, human_side, remaining_human_path) |> IO.inspect
  end

  # Base case - stop when we've hit "humn" and return the value
  def infer_operand(_, value, "humn", []), do: value
  @doc "Recursively infer the operand given a value and two sides, one of which can be calculated"
  def infer_operand(jobs, value, name, [human_side | human_path]) do
    job = jobs[name]
    # Non-human side
    other_side = if job.op1 == human_side, do: job.op2, else: job.op1
    new_value = cond do
      job.op == "+" -> value - get_value(jobs, other_side)
      job.op == "*" -> value / get_value(jobs, other_side)
      # reversing "-" and "/" depends on the order of operands since they're not commutative
      job.op == "-" && job.op1 == human_side -> value + get_value(jobs, job.op2)
      job.op == "-" && job.op2 == human_side -> get_value(jobs, job.op1) - value
      job.op == "/" && job.op1 == human_side -> value * get_value(jobs, job.op2)
      job.op == "/" && job.op2 == human_side -> get_value(jobs, job.op1) / value
    end
    # Recurse with next element in the "human" path
    infer_operand(jobs, new_value, human_side, human_path)
  end

  @doc "Parse the input into a Map of jobs"
  def parse_input(input) do
    input |> String.trim |> String.split("\n") |> Enum.map(fn line ->
      [key, val] = line |> String.split(": ")
      if !String.contains?(val, " ") do
        {key, %{type: "val", val: String.to_integer(val)}}
      else
        [op1, op, op2] = String.split(val, " ")
        {key, %{type: "op", op1: op1, op2: op2, op: op}}
      end
    end) |> Enum.into(%{})
  end

  @doc "Recursively calculate value based on job"
  def get_value(jobs, name) do
    job = jobs[name]
    cond do
      job.type == "val" -> job.val
      true -> op = case job.op do
        "+" -> &Kernel.+/2
        "-" -> &Kernel.-/2
        "*" -> &Kernel.*/2
        "/" -> &Kernel.//2
      end
      op.(get_value(jobs, job.op1), get_value(jobs, job.op2))
    end
  end
end

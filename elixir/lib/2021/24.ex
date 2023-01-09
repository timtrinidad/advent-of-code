import AOC

# https://adventofcode.com/2021/day/24
aoc 2021, 24 do
  @init_var %{"w" => 0, "x" => 0, "y" => 0, "z" => 0}

  def p1(input) do
    :ets.new(:cache, [:set, :named_table])
    parse_input(input) |> run_program(@init_var, [])
  end

  def p2(input) do
    parse_input(input)
  end

  def parse_input(input) do
    input |> String.trim() |> String.split("\n") |> Enum.with_index |> Enum.map(fn {line, idx} ->
      [inst | args] = line |> String.split(" ")
      {idx, inst, args}
    end)
  end

  def run_program([], variables, input_history), do: (if variables["z"] == 0, do: Integer.undigits(Enum.reverse(input_history)) |> IO.inspect, else: 0)
  def run_program([curr = {idx, inst, args} | next], variables, input_history) do
    cache = :ets.lookup(:cache, {idx, vars_cache_key(variables)})

    num = cond do
      length(cache) > 0 -> num = cache |> Enum.at(0) |> elem(1); num
      inst != "inp" ->
        variables = run_inst(curr, variables)
        run_program(next, variables, input_history)
      true ->
        9..1 |> Enum.map(fn i ->
          variables = Map.put(variables, Enum.at(args, 0), i)
          input_history = [i | input_history]
          run_program(next, variables, input_history)
        end) |> Enum.max
    end

    if length(cache) == 0, do: :ets.insert(:cache, {{idx, vars_cache_key(variables)}, num});

    num
  end

  def run_inst({_, inst, operands = [to_var, _]}, variables) do
    [op1, op2] = vars_to_nums(operands, variables)

    op = case inst do
      "add" -> &Kernel.+/2
      "mul" -> &Kernel.*/2
      "div" -> &div/2
      "mod" -> &rem/2
      "eql" -> fn (a, b) -> if a == b, do: 1, else: 0 end
    end

    Map.put(variables, to_var, op.(op1, op2))
  end


  def vars_to_nums([var1, var2], variables) do
    var1 = Map.get(variables, var1, 0)
    var2 = if Regex.match?(~r/^-?[0-9]+$/, var2), do: String.to_integer(var2), else: variables[var2]
    [var1, var2]
  end

  def vars_cache_key(variables), do: [variables["w"], variables["x"], variables["y"], variables["z"]]
end

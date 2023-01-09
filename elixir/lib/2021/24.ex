import AOC

# https://adventofcode.com/2021/day/24
aoc 2021, 24 do
  @init_var %{"w" => 0, "x" => 0, "y" => 0, "z" => 0}

  # Couldn't wrap my head around the problem by myself.
  # Used the method described in https://www.reddit.com/r/adventofcode/comments/rom5l5/2021_day_24pen_paper_monad_deparsed/
  # to calculate the numbers by hand, validated with this program.

  def p1(input) do
    program_input = 45_989_929_946_199 |> Integer.digits()
    variables = parse_input(input) |> run_program(program_input, @init_var)
    IO.puts("Is valid model number:")
    variables["z"] == 0
  end

  def p2(input) do
    program_input = 13_579_246_899_999 |> Integer.digits()
    variables = parse_input(input) |> run_program(program_input, @init_var)
    IO.puts("Is valid model number:")
    variables["z"] == 0
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [inst | args] = line |> String.split(" ")
      {inst, args}
    end)
  end

  def run_program([], _, variables), do: variables

  def run_program([curr | next], input, variables) do
    {input, variables} = run_inst(curr, input, variables)
    run_program(next, input, variables)
  end

  def run_inst({"inp", [var_name]}, [first_inp | rest_inp], variables) do
    variables = Map.put(variables, var_name, first_inp)
    {rest_inp, variables}
  end

  def run_inst({inst, operands = [to_var, _]}, input, variables) do
    [op1, op2] = vars_to_nums(operands, variables)

    op =
      case inst do
        "add" -> &Kernel.+/2
        "mul" -> &Kernel.*/2
        "div" -> &div/2
        "mod" -> &rem/2
        "eql" -> fn a, b -> if a == b, do: 1, else: 0 end
      end

    {input, Map.put(variables, to_var, op.(op1, op2))}
  end

  def vars_to_nums([var1, var2], variables) do
    var1 = Map.get(variables, var1, 0)

    var2 =
      if Regex.match?(~r/^-?[0-9]+$/, var2), do: String.to_integer(var2), else: variables[var2]

    [var1, var2]
  end
end

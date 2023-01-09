import AOC

# https://adventofcode.com/2021/day/24
aoc 2021, 24 do
  @init_var %{"w" => 0, "x" => 0, "y" => 0, "z" => 0}

  # Couldn't wrap my head around the problem by myself.
  # Used the method described in https://www.reddit.com/r/adventofcode/comments/rom5l5/2021_day_24pen_paper_monad_deparsed/
  # to build solver() and ran the parsed program to validate.

  def p1(input) do
    program_input = solver(input, true) |> IO.inspect() |> Integer.digits()
    variables = parse_input(input) |> run_program(program_input, @init_var)
    IO.puts("Is valid model number:")
    variables["z"] == 0
  end

  def p2(input) do
    program_input = solver(input, false) |> IO.inspect() |> Integer.digits()
    variables = parse_input(input) |> run_program(program_input, @init_var)
    IO.puts("Is valid model number:")
    variables["z"] == 0
  end

  @doc "Solver for the given input assuming structure outlined in aforementioned post"
  def solver(input, largest?) do
    # Split the full set of instructions into 14 sections, taking the values of rows 5, 6, 16 and their values
    # since all other instructions are the same across each of the 14 sections
    instructions =
      input
      |> String.split("\n")
      |> Enum.chunk_every(18)
      |> Enum.map(fn chunk ->
        chunk
        |> Enum.with_index(1)
        |> Enum.filter(fn {_, idx} -> idx in [5, 6, 16] end)
        |> Enum.map(fn {line, _} ->
          line |> String.split(" ") |> Enum.at(-1) |> String.to_integer()
        end)
      end)
      |> Enum.with_index()

    # Go through and match up, via a stack, cases where row 5 is 1 and 26. For example, if we have the following:
    #  1: 1
    #  2: 1
    #  3: 26
    #  4: 26
    # We should match up rows 3,2 and rows 4,1.
    {_, pairs} =
      instructions
      |> Enum.reduce({[], []}, fn {[a, b, c], idx}, {stack, pairs} ->
        if a == 1 do
          # Add to stack
          {[{idx, c} | stack], pairs}
        else
          # Found a match. Remove from stack and add to pairs, tracking the sum of
          # B for this row and C for the matched row. Results in
          # a list of matched rows as tuples like {idx1, idx2, diff}
          [{matched_idx, matched_val} | stack] = stack
          pairs = [{matched_idx, idx, matched_val + b} | pairs]
          {stack, pairs}
        end
      end)

    # For each pair, solve for each of the digits.
    pairs
    |> Enum.reduce([], fn {idx1, idx2, diff}, acc ->
      cond do
        # Solving for largest case where the difference is positive - set
        # the larger of the two digits to 9 and subtract the diff to get the other digit
        largest? && diff > 0 -> [{idx2, 9}, {idx1, 9 - diff} | acc]
        # Same, but if difference is negative
        largest? -> [{idx1, 9}, {idx2, 9 + diff} | acc]
        # Solving for smallest case. Same as above, but set smaller of two digits to 1
        diff > 0 -> [{idx2, 1 + diff}, {idx1, 1} | acc]
        true -> [{idx1, 1 - diff}, {idx2, 1} | acc]
      end
    end)
    |> Enum.sort()
    |> Enum.map(&elem(&1, 1))
    |> Integer.undigits()
  end

  @doc "Parse the input into a list of tuples like {add, [x, 1]}"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [inst | args] = line |> String.split(" ")
      {inst, args}
    end)
  end

  # Base case - no more instructions to run
  def run_program([], _, variables), do: variables

  @doc "Recursively run the program until there are no more remaining instructions. Keep track of the updated variable values and inputs"
  def run_program([curr | next], input, variables) do
    {input, variables} = run_inst(curr, input, variables)
    run_program(next, input, variables)
  end

  # Run the "inp" instruction - set the value of the variable and pop the inputs from the stack.
  def run_inst({"inp", [var_name]}, [first_inp | rest_inp], variables) do
    variables = Map.put(variables, var_name, first_inp)
    {rest_inp, variables}
  end

  @doc "Run a specific instruction"
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

    # Run the operation and return the updated variable values
    {input, Map.put(variables, to_var, op.(op1, op2))}
  end

  @doc "Convert operands to integers"
  def vars_to_nums([var1, var2], variables) do
    var1 = Map.get(variables, var1, 0)

    var2 =
      if Regex.match?(~r/^-?[0-9]+$/, var2), do: String.to_integer(var2), else: variables[var2]

    [var1, var2]
  end
end

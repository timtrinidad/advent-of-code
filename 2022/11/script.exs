# https://adventofcode.com/2022/day/11

defmodule DayEleven do
  @doc "Simulate the monkey tossing for a given number of rounds. Returns monkeys with the items they have and the number of inspections."
  def simulate_rounds(input, num_rounds, worry_divisor) do
    monkey_attributes = parse_input(input)
    # Create a monkey/round list
    monkey_rounds = for _ <- 1..num_rounds do
      for j <- 1..length(monkey_attributes), do: j-1
    end |> Enum.concat
    exec_inspection(monkey_attributes, worry_divisor, monkey_rounds)
  end

  @doc "Parse the input (regex the strings) into a map."
  def parse_input(input) do
    input
      |> String.trim
      |> String.split("\n\n")
      |> Enum.map(fn x ->
        regex_match = Regex.named_captures(
          ~r/Monkey (?<monkey_num>\d+):\n  Starting items: (?<items>.+)\n  Operation: new = old (?<operation>.+)\n  Test: divisible by (?<test_divisible_by>.+)\n    If true: throw to monkey (?<to_monkey_true>\d+)\n    If false: throw to monkey (?<to_monkey_false>\d+)/,
          x
        )
        if regex_match == nil, do: raise "Unexpected input: #{x}"
        regex_match
        # Convert into ints or lists pof ints
        |> Map.update!("items", fn x -> x |> String.split(", ") |> Enum.map(&String.to_integer/1) end)
        |> Map.update!("monkey_num", &String.to_integer/1)
        |> Map.update!("to_monkey_true", &String.to_integer/1)
        |> Map.update!("to_monkey_false", &String.to_integer/1)
        |> Map.update!("test_divisible_by", &String.to_integer/1)
        # Add a new "num_inspections" attribute
        |> put_in(["num_inspections"], 0)
      end)
  end

  @doc "Handle a single inspection"
  def exec_inspection(monkeys, worry_divisor, [curr_monkey_id | next_monkey_ids]) do
    # Get the current monkey and items it has
    curr_monkey = monkeys |> Enum.at(curr_monkey_id)
    curr_monkey_items = curr_monkey["items"]

    # Reign in big numbers based on the lowest common multiple of all the possible test divisors
    worry_mod_divisor = monkeys |> Enum.map(fn x -> x["test_divisible_by"] end) |> Enum.product

    cond do
      length(curr_monkey_items) == 0 ->
        # No more items - move onto next monkey
        exec_inspection(monkeys, worry_divisor, next_monkey_ids)
      true ->
        [curr_item | next_items] = curr_monkey_items
        # Process worry levels based on rules
        curr_item = curr_item
          |> operate(curr_monkey["operation"])
          |> Kernel./(worry_divisor)
          |> trunc
          |> rem(worry_mod_divisor)
        # Depending on whether or not the new worry level is divisible, send to one of two different monkeys
        to_monkey_id = if rem(curr_item, curr_monkey["test_divisible_by"]) == 0 do
          curr_monkey["to_monkey_true"]
        else
          curr_monkey["to_monkey_false"]
        end

        # "Update" the monkey attributes with new item lists and new inspection counts
        to_monkey = monkeys
          |> Enum.at(to_monkey_id)
          |> update_in(["items"], fn x -> x ++ [curr_item] end)
        curr_monkey = curr_monkey
          |> put_in(["items"], next_items)
          |> update_in(["num_inspections"], fn x -> x + 1 end)
        monkeys = monkeys
          |> List.replace_at(to_monkey_id, to_monkey)
          |> List.replace_at(curr_monkey_id, curr_monkey)

        # Recurse for the same monkey since it still has items
        exec_inspection(monkeys, worry_divisor, [curr_monkey_id] ++ next_monkey_ids)
    end
  end
  # Base case - no more monkey IDs left
  def exec_inspection(monkeys, _, []), do: monkeys

  def operate(curr_value, operation_string) do
    [operator, operand] = String.split(operation_string, " ")
    operand = if operand == "old", do: curr_value, else: String.to_integer(operand)
    case operator do
      "+" -> curr_value + operand
      "-" -> curr_value - operand
      "*" -> curr_value * operand
      "/" -> curr_value / operand
    end
  end
end

{:ok, input} = File.read('2022/11/input.txt')

num_inspections_sorted = DayEleven.simulate_rounds(input, 20, 3)
  |> Enum.map(fn x -> x["num_inspections"] end)
  |> IO.inspect(charlists: :as_lists, label: "# Inspections per monkey")
  |> Enum.sort
  |> Enum.reverse
IO.inspect(Enum.at(num_inspections_sorted, 0) * Enum.at(num_inspections_sorted, 1), label: "Monkey business level")

num_inspections_sorted = DayEleven.simulate_rounds(input, 10000, 1)
   |> Enum.map(fn x -> x["num_inspections"] end)
   |> IO.inspect(charlists: :as_lists, label: "# Inspections per monkey")
   |> Enum.sort
   |> Enum.reverse
IO.inspect(Enum.at(num_inspections_sorted, 0) * Enum.at(num_inspections_sorted, 1), label: "Monkey business level")

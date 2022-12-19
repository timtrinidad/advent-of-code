import AOC

aoc 2022, 19 do
  def p1(input) do
    parse_input(input)
    |> IO.inspect()
    |> simulate_blueprints(24)
    |> Enum.with_index()
    |> Enum.map(fn {{storage, _}, idx} ->
      # Multiply each geode count by the ID (idx + 1) and sum
      IO.inspect({idx, storage})
      (idx + 1) * storage.geode
    end)
    |> Enum.sum()
    |> IO.inspect(limit: :infinity)
  end

  def p2(input) do
    parse_input(input)
    # First 3 blueprints
    |> Enum.slice(0..2)
    |> IO.inspect()
    |> simulate_blueprints(32)
    # Multiply all geode count together
    |> Enum.map(fn {storage, _} ->
      IO.inspect({storage})
      storage.geode
    end)
    |> Enum.product()
    |> IO.inspect(limit: :infinity)
  end

  @doc "Simulate a list of blueprints for a given number of minutes"
  def simulate_blueprints(blueprints, minutes) do
    # Shared state caches
    :ets.new(:cache, [:set, :named_table])
    :ets.new(:max, [:set, :named_table])

    robots = %{ore: 1, clay: 0, obsidian: 0, geode: 0}
    storage = %{ore: 0, clay: 0, obsidian: 0, geode: 0}

    blueprints
    |> Enum.map(fn blueprint ->
      IO.inspect(blueprint)
      # Clear caches before simulating
      :ets.delete_all_objects(:cache)
      :ets.delete_all_objects(:max)
      simulate_minute(blueprint, robots, storage, 0, [], minutes)
    end)
  end

  @doc "Parse the input into blueprints"
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [
        id,
        ore_ore_cost,
        clay_ore_cost,
        obsidian_ore_cost,
        obsidian_clay_cost,
        geode_ore_cost,
        geode_obsidian_cost
      ] =
        Regex.run(
          ~r/Blueprint (\d+): .+?(\d+) ore. .+?(\d+) ore. .+?(\d+) ore and (\d+) clay. .+? (\d+) ore and (\d+) obsidian./,
          line
        )
        |> tl
        |> Enum.map(&String.to_integer/1)

      %{
        id: id,
        ore: %{ore: ore_ore_cost},
        clay: %{ore: clay_ore_cost},
        obsidian: %{ore: obsidian_ore_cost, clay: obsidian_clay_cost},
        geode: %{ore: geode_ore_cost, obsidian: geode_obsidian_cost},
        # Keep track of the max number of each item needed across all robots
        max_needed: %{
          ore: Enum.max([ore_ore_cost, clay_ore_cost, obsidian_ore_cost, geode_ore_cost]),
          clay: obsidian_clay_cost,
          obsidian: geode_obsidian_cost
        }
      }
    end)
  end

  # Base case - when we hit 0 minutes, determine if this was the higher number of geodes encountered, and return the storage counts"
  def simulate_minute(_, _, storage, _, history, 0) do
    num_geodes = storage.geode
    curr_max = :ets.lookup(:max, :max_geodes)
    curr_max = if length(curr_max) > 0, do: curr_max |> Enum.at(0) |> elem(1), else: 0
    if num_geodes > curr_max, do: :ets.insert(:max, {:max_geodes, num_geodes})
    {storage, history}
  end

  @doc "Simulate a blueprint for the given number of minutes. Recursive until minute == 0"
  def simulate_minute(blueprint, robots, storage, should_buy, history, minute) do
    history = history ++ [should_buy]

    # Look up to see if this specific request is cached to prevent further recursion branches
    cache_key =
      "#{robots.ore},#{robots.clay},#{robots.obsidian},#{robots.geode}|#{storage.ore},#{storage.clay},#{storage.obsidian},#{storage.geode}|#{should_buy}|#{minute}"

    cache = :ets.lookup(:cache, cache_key)

    # Look up the max number of geodes encountered to far
    curr_max_geodes = :ets.lookup(:max, :max_geodes)

    curr_max_geodes =
      if length(curr_max_geodes) > 0, do: curr_max_geodes |> Enum.at(0) |> elem(1), else: 0

    max_possible_geodes = Enum.sum(robots.geode..(robots.geode + minute - 1)) + storage.geode

    if minute > 14, do: IO.inspect({history, curr_max_geodes})

    cond do
      # Cached. Parse out the integer value into a map and return the cached value without further recursion branches
      length(cache) > 0 ->
        cache_val = cache |> Enum.at(0) |> elem(1)
        num_geodes = div(cache_val, 1_000_000)
        num_obsidian = div(rem(cache_val, 1_000_000), 10000)
        num_clay = div(rem(cache_val, 10000), 100)
        num_ore = rem(cache_val, 100)

        {%{geode: num_geodes, obsidian: num_obsidian, clay: num_clay, ore: num_ore},
         history ++ ["cached"]}

      # Given the number of remaining minutes, it's impossible to get more than the max number
      # of geodes encountered so far. Prevent further recursion.
      max_possible_geodes < curr_max_geodes ->
        {storage, history ++ ["wont_beat_max_#{max_possible_geodes}_#{curr_max_geodes}"]}

      true ->
        # Convenience function to buy a robot
        buy_robot = fn type -> buy_robot(type, blueprint, robots, storage) end

        # Depending on the recursion branch, if possible, buy the specified robot.
        {new_robots, storage} =
          case should_buy do
            4 when minute > 1 ->
              buy_robot.(:geode)

            3 when minute > 1 ->
              buy_robot.(:obsidian)

            2 when minute > 1 ->
              buy_robot.(:clay)

            1 when minute > 1 ->
              buy_robot.(:ore)

            _ ->
              {robots, storage}
          end

        # Based on the number of robots (before purchase), increase the storage amounts
        storage =
          robots
          |> Enum.reduce(storage, fn {robot_type, num_robots}, acc ->
            update_in(acc[robot_type], fn x -> x + num_robots end)
          end)

        options = []

        # For each possible next robot to purchase - create recursion branch
        options =
          [{:geode, 4}, {:obsidian, 3}, {:clay, 2}, {:ore, 1}]
          |> Enum.reduce(options, fn {type, should_buy}, acc ->
            # Determine if there are any more robots needed for this type given the number of minutes left
            no_more_needed =
              type != :geode &&
                robots[type] * minute + storage[type] >= minute * blueprint.max_needed[type]

            # If more are needed and we can buy it based on our storage amounts, create recursion branch
            if !no_more_needed && can_buy(type, storage, blueprint),
              do: acc ++ [should_buy],
              else: acc
          end)

        # Add branch to not buy any robot if we don't already have too many of all resources
        options =
          if storage.ore < blueprint.max_needed.ore || storage.clay < blueprint.max_needed.clay ||
               storage.obsidian < blueprint.max_needed.obsidian do
            options ++ [0]
          else
            options
          end

        # For each branch, execute. Choose the result of the branch with the highest number of geodes.
        {storage, history} =
          options
          |> Enum.map(fn should_buy ->
            simulate_minute(blueprint, new_robots, storage, should_buy, history, minute - 1)
          end)
          |> Enum.max_by(fn {x, _} -> x.geode end)

        # Store result in cache
        cache_val =
          storage.ore * 1 + storage.clay * 100 + storage.obsidian * 10000 +
            storage.geode * 1_000_000

        if :ets.info(:cache)[:size] > 10_000_000, do: :ets.delete_all_objects(:cache)
        :ets.insert(:cache, {cache_key, cache_val})
        {storage, history}
    end
  end

  @doc "Determine whether or not a robot can be purchased with the given storage amounts"
  def can_buy(type, storage, blueprint) do
    case type do
      :geode ->
        storage.obsidian >= blueprint.geode.obsidian && storage.ore >= blueprint.geode.ore

      :obsidian ->
        storage.clay >= blueprint.obsidian.clay && storage.ore >= blueprint.obsidian.ore

      :clay ->
        storage.ore >= blueprint.clay.ore

      :ore ->
        storage.ore >= blueprint.ore.ore
    end
  end

  @doc "Buy the specified robot and decrease storage amounts as necessary"
  def buy_robot(type, blueprint, robots, storage) do
    update_storage = fn robot_type, types ->
      Enum.reduce(types, storage, fn mat, acc ->
        update_in(acc[mat], &(&1 - blueprint[robot_type][mat]))
      end)
    end

    case type do
      :ore ->
        {update_in(robots.ore, &(&1 + 1)), update_storage.(:ore, [:ore])}

      :clay ->
        {update_in(robots.clay, &(&1 + 1)), update_storage.(:clay, [:ore])}

      :obsidian ->
        {update_in(robots.obsidian, &(&1 + 1)), update_storage.(:obsidian, [:ore, :clay])}

      :geode ->
        {update_in(robots.geode, &(&1 + 1)), update_storage.(:geode, [:ore, :obsidian])}
    end
  end
end

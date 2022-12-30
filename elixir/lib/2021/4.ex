import AOC

aoc 2021, 4 do
  def p1(input) do
    {last_num, winning_board} = parse_input(input) |> call_numbers

    sum_uncalled_nums =
      winning_board
      |> Enum.reduce(0, fn {num, spot}, acc ->
        if spot.called, do: acc, else: acc + num
      end)

    last_num * sum_uncalled_nums
  end

  def p2(_input) do
  end

  @doc "Split the input into a list of ints (called numbers) and a list of boards. Each board is a map of numbers with attributes 'called' and 'pos' with an {x, y}"
  def parse_input(input) do
    [called_numbers | boards] = input |> String.trim() |> String.split("\n\n")

    called_numbers = called_numbers |> String.split(",") |> Enum.map(&String.to_integer/1)

    boards =
      boards
      |> Enum.map(fn board ->
        board
        # Split each board into rows
        |> String.split("\n")
        |> Enum.with_index()
        |> Enum.reduce(%{}, fn {row, y}, acc ->
          # For each row, split into individual numbers.
          String.trim(row)
          |> String.split([" ", "  "])
          |> Enum.with_index()
          |> Enum.reduce(acc, fn {col, x}, acc ->
            # Put into a single map keyed by the number with attributes "called" and "pos"
            Map.put(acc, String.to_integer(col), %{called: false, pos: {x, y}})
          end)
        end)
      end)

    {called_numbers, boards}
  end

  @doc "Go through each called number and return the winning board and last called num"
  def call_numbers({numbers, boards}) do
    numbers
    |> Enum.reduce_while(boards, fn num, boards ->
      boards =
        boards
        |> Enum.map(fn board ->
          # Update each board and set "called" to true if the board has the number
          if Map.has_key?(board, num), do: put_in(board, [num, :called], true), else: board
        end)

      # If there is a winning board, stop. Otherwise, continue.
      winning_board = find_winning_board(boards)
      if winning_board, do: {:halt, {num, winning_board}}, else: {:cont, boards}
    end)
  end

  @doc "Go through each board to see if one is a winner (i.e. the same col or row is called 5 times)"
  def find_winning_board(boards) do
    boards
    # Go through each board and find if it's winning
    |> Enum.find(fn board ->
      # Get all numbers (and their position) that are called
      called_nums = board |> Enum.filter(fn {_, spot} -> spot.called end)

      # Get the number of times each column is seen for all called numbers
      called_col_freq =
        called_nums
        |> Enum.map(fn {_, spot} -> elem(spot.pos, 0) end)
        |> Enum.frequencies()
        |> Map.values()

      # Same with rows
      called_row_freq =
        called_nums
        |> Enum.map(fn {_, spot} -> elem(spot.pos, 1) end)
        |> Enum.frequencies()
        |> Map.values()

      # Is a winning board if the max frequency of row or col numbers is 5
      (length(called_col_freq) > 0 && Enum.max(called_col_freq) == 5) ||
        (length(called_row_freq) > 0 && Enum.max(called_row_freq) == 5)
    end)
  end
end

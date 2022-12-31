import AOC

aoc 2021, 4 do
  def p1(input) do
    parse_input(input) |> call_numbers(:first) |> calc_board_score
  end

  def p2(input) do
    parse_input(input) |> call_numbers(:last) |> calc_board_score
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
  def call_numbers({numbers, boards}, type) do
    numbers
    |> Enum.reduce_while(boards, fn num, boards ->
      boards =
        boards
        |> Enum.map(fn board ->
          # Update each board and set "called" to true if the board has the number
          if Map.has_key?(board, num), do: put_in(board, [num, :called], true), else: board
        end)

      case type do
        :first ->
          # Halt once we have any winning board (first one)
          boards_by_status = find_winning_boards(boards)

          if Map.has_key?(boards_by_status, :winners),
            do: {:halt, {num, hd(boards_by_status.winners)}},
            else: {:cont, boards}

        :last ->
          # Halt once there are no more losing boards
          boards_by_status = find_winning_boards(boards)

          if !Map.has_key?(boards_by_status, :losers),
            do: {:halt, {num, hd(boards_by_status.winners)}},
            else: {:cont, boards_by_status.losers}
      end
    end)
  end

  @doc "Go through each board to see if one is a winner (i.e. the same col or row is called 5 times)"
  def find_winning_boards(boards) do
    boards
    # Go through each board and find if it's winning
    |> Enum.group_by(fn board ->
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
      if (length(called_col_freq) > 0 && Enum.max(called_col_freq) == 5) ||
           (length(called_row_freq) > 0 && Enum.max(called_row_freq) == 5),
         do: :winners,
         else: :losers
    end)
  end

  def calc_board_score({last_num, board}) do
    sum_uncalled_nums =
      board
      |> Enum.reduce(0, fn {num, spot}, acc ->
        if spot.called, do: acc, else: acc + num
      end)

    last_num * sum_uncalled_nums
  end
end

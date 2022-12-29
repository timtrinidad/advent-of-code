import AOC

aoc 2022, 2 do
  @opponent_hands %{A: :rock, B: :paper, C: :scissors}
  @my_hands %{X: :rock, Y: :paper, Z: :scissors}
  @my_outcomes %{X: :lose, Y: :draw, Z: :win}

  def p1(input) do
    roundScoresA = processRounds(input, :hand)
    IO.inspect(Enum.sum(roundScoresA), label: "Total score if second column signifies hand")
  end

  def p2(input) do
    roundScoresB = processRounds(input, :outcome)
    IO.inspect(Enum.sum(roundScoresB), label: "Total score if second column signifies outcome")
  end

  @doc "For each line, calculate the round score"
  def processRounds(input, secondColumnType) do
    String.trim(input)
    |> String.split("\n")
    |> Enum.map(fn x ->
      String.split(x, " ")
      |> Enum.map(&String.to_atom/1)
      |> getRoundScore(secondColumnType)
    end)
  end

  @doc """
     Sum the winner score (win/loss/tie) and the score of the hand I play.

     The second argument can be:
        :hand - to signify that X/Y/Z corresponds to the hand which should be played
        :outcome - to signify that X/Y/Z corresponds the desired outcome
  """
  def getRoundScore([opponent, me], secondColumnType) do
    opponent_hand = @opponent_hands[opponent]

    my_hand =
      if secondColumnType == :hand,
        do: @my_hands[me],
        else: getMyHand(opponent_hand, @my_outcomes[me])

    getWinnerScore(opponent_hand, my_hand) + getMyScore(my_hand)
  end

  @doc "Determine the hand I should play based on the desired outcome and the opponent's hand"
  def getMyHand(opponent, my_outcome) do
    case {opponent, my_outcome} do
      # For draw, play the opponent's hand
      {x, :draw} -> x
      {:rock, :win} -> :paper
      {:rock, :lose} -> :scissors
      {:paper, :win} -> :scissors
      {:paper, :lose} -> :rock
      {:scissors, :win} -> :rock
      {:scissors, :lose} -> :paper
    end
  end

  @doc "Return a score based on the hands played"
  def getWinnerScore(opponent, me) do
    case {opponent, me} do
      # Scenarios where I lose: 0 points
      {:rock, :scissors} -> 0
      {:paper, :rock} -> 0
      {:scissors, :paper} -> 0
      # Scenarios where I win: 6 points
      {:rock, :paper} -> 6
      {:paper, :scissors} -> 6
      {:scissors, :rock} -> 6
      # All others (tie)
      _ -> 3
    end
  end

  @doc "Return the score based on my hand"
  def getMyScore(me) do
    case me do
      :rock -> 1
      :paper -> 2
      :scissors -> 3
    end
  end
end

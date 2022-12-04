app "day2"
    packages { pf: "../../../../roc_nightly-macos_11_x86_64-2022-11-23-0ac6fe7/examples/cli/cli-platform/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Program.{ ExitCode, Program },
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

Round : { left : Hand, right : Hand }
Hand : [Rock, Paper, Scissors]

RoundPlan : { op : Hand, outcome : Outcome }
Outcome : [Lose, Draw, Win]

inputPath : Path.Path
inputPath = Path.fromStr "part1.txt"

main : Program
main = Program.quick mainTask

mainTask : Task {} [] [Read [File], Write [Stderr, Stdout]]
mainTask =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        rounds <- inputString |> parsePart1 |> Task.fromResult |> Task.await
        p1 = calculateRoundsScore rounds

        roundPlans <- inputString |> parsePart2 |> Task.fromResult |> Task.await
        p2 = calculateRoundPlansScore roundPlans

        _ <- Stdout.line "Part1: \(p1)" |> Task.await
        Stdout.line "Part2: \(p2)"

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.succeed {}
            Err err ->
                when err is
                    FileReadErr _ _ -> Stderr.line "Error reading file"
                    FileReadUtf8Err _ _ -> Stderr.line "Error with path"
                    InvalidRoundErr str -> Stderr.line "InvalidRoundErr: \(str)"
                    InvalidHandErr str -> Stderr.line "InvalidHandErr: \(str)"
                    InvalidRoundPlanErr str -> Stderr.line "InvalidRoundPlanErr: \(str)"
                    InvalidOutcomeErr str -> Stderr.line "InvalidOutcomeErr: \(str)"

parsePart1 : Str -> Result (List Round) [InvalidRoundErr Str, InvalidHandErr Str]
parsePart1 = \inputString ->
    inputString
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseRound

parseRound : Str -> Result Round [InvalidRoundErr Str]
parseRound = \round ->
    when Str.split round " " is
        [leftHand, rightHand] ->
            left <- leftHand |> parseHand |> Result.try
            right <- rightHand |> parseHand |> Result.try
            Ok { left, right }

        _ -> Err (InvalidRoundErr round)

parseHand : Str -> Result Hand [InvalidHandErr Str]
parseHand = \hand ->
    when hand is
        "A" | "X" -> Ok Rock
        "B" | "Y" -> Ok Paper
        "C" | "Z" -> Ok Scissors
        _ -> Err (InvalidHandErr hand)

parsePart2 : Str -> Result (List RoundPlan) [InvalidRoundPlanErr Str, InvalidOutcomeErr Str]
parsePart2 = \inputString ->
    inputString
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseRoundPlan

parseRoundPlan : Str -> Result RoundPlan [InvalidRoundPlanErr Str]
parseRoundPlan = \plan ->
    when Str.split plan " " is
        [opponentHand, outcomePlan] ->
            op <- opponentHand |> parseHand |> Result.try
            outcome <- outcomePlan |> parseOutcome |> Result.try
            Ok { op, outcome }

        _ -> Err (InvalidRoundPlanErr plan)

parseOutcome : Str -> Result Outcome [InvalidOutcomeErr Str]
parseOutcome = \outcome ->
    when outcome is
        "X" -> Ok Lose
        "Y" -> Ok Draw
        "Z" -> Ok Win
        _ -> Err (InvalidOutcomeErr outcome)

calculateRoundsScore : List Round -> Str
calculateRoundsScore = \rounds ->
    tally = \acc, round -> round |> calculateRound |> Num.add acc

    List.walk rounds 0 tally
    |> Num.toStr

# "1 for Rock, 2 for Paper, and 3 for Scissors"
# "0 if you lost, 3 if the round was a draw, and 6 if you won"
calculateRound : Round -> Nat
calculateRound = \round ->
    when round is
        { left: Rock, right: Rock } -> 1 + 3
        { left: Paper, right: Rock } -> 1 + 0
        { left: Scissors, right: Rock } -> 1 + 6
        { left: Rock, right: Paper } -> 2 + 6
        { left: Paper, right: Paper } -> 2 + 3
        { left: Scissors, right: Paper } -> 2 + 0
        { left: Rock, right: Scissors } -> 3 + 0
        { left: Paper, right: Scissors } -> 3 + 6
        { left: Scissors, right: Scissors } -> 3 + 3

calculateRoundPlansScore : List RoundPlan -> Str
calculateRoundPlansScore = \plans ->
    tally = \acc, plan -> plan |> calculateRoundPlan |> Num.add acc

    List.walk plans 0 tally
    |> Num.toStr

# "1 for Rock, 2 for Paper, and 3 for Scissors"
# "0 if you lost, 3 if the round was a draw, and 6 if you won"
calculateRoundPlan : RoundPlan -> Nat
calculateRoundPlan = \plan ->
    when plan is
        { op: Rock, outcome: Lose } -> 3 + 0
        { op: Paper, outcome: Lose } -> 1 + 0
        { op: Scissors, outcome: Lose } -> 2 + 0
        { op: Rock, outcome: Draw } -> 1 + 3
        { op: Paper, outcome: Draw } -> 2 + 3
        { op: Scissors, outcome: Draw } -> 3 + 3
        { op: Rock, outcome: Win } -> 2 + 6
        { op: Paper, outcome: Win } -> 3 + 6
        { op: Scissors, outcome: Win } -> 1 + 6

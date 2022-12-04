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

inputPath : Path.Path
inputPath = Path.fromStr "part1.txt"

main : Program
main = Program.quick mainTask

mainTask : Task {} [] [Read [File], Write [Stderr, Stdout]]
mainTask =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        rounds <- inputString |> parse |> Task.fromResult |> Task.await
        p1 = calculateScore rounds

        _ <- Stdout.line "Part1: \(p1)" |> Task.await
        Stdout.line "Part2: todo"

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.succeed {}
            Err err ->
                when err is
                    FileReadErr _ _ -> Stderr.line "Error reading file"
                    FileReadUtf8Err _ _ -> Stderr.line "Error with path"
                    InvalidRoundErr str -> Stderr.line "InvalidRoundErr: \(str)"
                    InvalidHandErr str -> Stderr.line "InvalidHandErr: \(str)"

parse : Str -> Result (List Round) [InvalidRoundErr Str, InvalidHandErr Str]
parse = \inputString ->
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

calculateScore : List Round -> Str
calculateScore = \rounds ->
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

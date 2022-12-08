app "day5"
    packages { pf: "../../../../basic-cli/src/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

CrateState : Dict Nat (List Str)
Move : { count : Nat, from : Nat, to : Nat }

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

# Two hard to parse/need more practice with Roc parsers
#
# [T]     [D]         [L]
# [R]     [S] [G]     [P]         [H]
# [G]     [H] [W]     [R] [L]     [P]
# [W]     [G] [F] [H] [S] [M]     [L]
# [Q]     [V] [B] [J] [H] [N] [R] [N]
# [M] [R] [R] [P] [M] [T] [H] [Q] [C]
# [F] [F] [Z] [H] [S] [Z] [T] [D] [S]
# [P] [H] [P] [Q] [P] [M] [P] [F] [D]
#  1   2   3   4   5   6   7   8   9
startingCrates =
    Dict.single 1 ["T", "R", "G", "W", "Q", "M", "F", "P"]
    |> Dict.insert 2 ["R", "F", "H"]
    |> Dict.insert 3 ["D", "S", "H", "G", "V", "R", "Z", "P"]
    |> Dict.insert 4 ["G", "W", "F", "B", "P", "H", "Q"]
    |> Dict.insert 5 ["H", "J", "M", "S", "P"]
    |> Dict.insert 6 ["L", "P", "R", "S", "H", "T", "Z", "M"]
    |> Dict.insert 7 ["L", "M", "N", "H", "T", "P"]
    |> Dict.insert 8 ["R", "Q", "D", "F"]
    |> Dict.insert 9 ["H", "P", "L", "N", "C", "S", "D"]
# startingCrates =
#     Dict.single 1 ["A", "B"]
#     |> Dict.insert 2 ["C"]
#     |> Dict.insert 3 ["D"]
main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        # inputString = "move 1 from 3 to 1"
        moves <- parseMoves inputString |> Task.fromResult |> Task.await
        # st = List.walk moves "" \s, { count: count, from: from, to: to } ->
        #     Str.joinWith [s, Num.toStr count, Num.toStr from, Num.toStr to] ""
        # _ <- Stdout.line "St: \(st)" |> Task.await
        final <- applyMoves moves startingCrates |> Task.fromResult |> Task.await
        # dict = Dict.walk final "" \sd, k, v ->
        #     Str.concat sd (List.walk v "" \sl, elem -> Str.concat sl elem)
        # _ <- Stdout.line "Dict: \(dict)" |> Task.await
        topCrates = getTopCrates final

        Stdout.line "Answer: \(topCrates)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            InvalidMove | InvalidNumStr | KeyNotFound -> Stderr.line "Parsing error"

parseMoves : Str -> Result (List Move) [InvalidMove]
parseMoves = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseMove

# example: "move 3 from 8 to 9"
parseMove : Str -> Result Move [InvalidMove]
parseMove = \input ->
    tokens = Str.split input " "

    when tokens is
        [_, countStr, _, fromStr, _, toStr] ->
            count <- Str.toNat countStr |> Result.try
            from <- Str.toNat fromStr |> Result.try
            to <- Str.toNat toStr |> Result.try

            Ok { count, from, to }

        _ -> Err InvalidMove

applyMoves : List Move, CrateState -> Result CrateState [KeyNotFound]
applyMoves = \moves, crates ->
    moves
    |> List.walkTry crates \newCrates, move ->
        applyMove move newCrates

applyMove : Move, CrateState -> Result CrateState [KeyNotFound]
applyMove = \{ count: count, from: from, to: to }, crates ->
    fromCrates <- Dict.get crates from |> Result.try
    toCrates <- Dict.get crates to |> Result.try
    { before: toMove, others: newFrom } = List.split fromCrates (count)
    # Part1:
    # newTo = List.concat (List.reverse toMove) toCrates
    newTo = List.concat toMove toCrates

    crates
    |> Dict.insert from newFrom
    |> Dict.insert to newTo
    |> Ok

getTopCrates : CrateState -> Str
getTopCrates = \crates ->
    Dict.walk crates "" \topCrates, _, stack ->
        crate = List.first stack

        when crate is
            Ok str -> Str.concat topCrates str
            Err ListWasEmpty -> topCrates

# v = Dict.insert Dict.empty 1 "one"
# v2 = Dict.get v 1
# Result.withDefault v2 "zxv"
# v3 = Dict.insert v 1 "blah"
# v4 = Dict.get v3 1
# Result.withDefault v4 "zxv"

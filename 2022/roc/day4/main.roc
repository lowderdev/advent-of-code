app "day4"
    packages { pf: "../../../../basic-cli/src/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

Range : { start : Nat, end : Nat }
RangePair : { a : Range, b : Range }

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        rangePairs <- inputString |> parse |> Task.fromResult |> Task.await
        fullyContainedRanges = rangePairs |> processRangePairs |> Num.toStr

        Stdout.line "Part1: \(fullyContainedRanges)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error with path"
            InvalidFilename _ _ _ -> Stderr.line "Invalid file name"
            InvalidRangePair | InvalidRange | InvalidNumStr -> Stderr.line "Invalid input"

parse : Str -> Result (List RangePair) [InvalidRangePair, InvalidRange, InvalidNumStr]
parse = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseRangePair

parseRangePair : Str -> Result RangePair [InvalidRangePair, InvalidRange, InvalidNumStr]
parseRangePair = \pair ->
    ranges <- pair
        |> Str.split ","
        |> List.mapTry parseRange
        |> Result.try

    when ranges is
        [a, b] -> Ok { a, b }
        _ -> Err InvalidRangePair

parseRange : Str -> Result Range [InvalidRange]
parseRange = \range ->
    nums = range |> Str.split "-"

    when nums is
        [startStr, endStr] ->
            start <- Str.toNat startStr |> Result.try
            end <- Str.toNat endStr |> Result.try

            Ok { start, end }

        _ -> Err InvalidRange

processRangePairs : List RangePair -> Nat
processRangePairs = \rangePairs ->
    rangePairs
    |> List.keepIf \{ a, b } -> rangeContainsRange a b || rangeContainsRange b a
    |> List.len

rangeContainsRange : Range, Range -> Bool
rangeContainsRange = \a, b ->
    a.start <= b.start && a.end >= b.end

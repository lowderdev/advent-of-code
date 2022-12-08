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
        containedRangeCount = rangePairs |> countContainedRangePairs |> Num.toStr
        overlappingRangeCount = rangePairs |> countOverlappingRangePairs |> Num.toStr

        _ <- Stdout.line "Part1: \(containedRangeCount)" |> Task.await
        Stdout.line "Part2: \(overlappingRangeCount)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
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

parseRange : Str -> Result Range [InvalidRange, InvalidNumStr]
parseRange = \range ->
    nums = range |> Str.split "-"

    when nums is
        [startStr, endStr] ->
            start <- Str.toNat startStr |> Result.try
            end <- Str.toNat endStr |> Result.try

            Ok { start, end }

        _ -> Err InvalidRange

countContainedRangePairs : List RangePair -> Nat
countContainedRangePairs = \rangePairs ->
    List.countIf rangePairs rangeContainsRange

rangeContainsRange : RangePair -> Bool
rangeContainsRange = \{ a, b } ->
    aContainsB = a.start <= b.start && a.end >= b.end
    bContainsA = b.start <= a.start && b.end >= a.end

    aContainsB || bContainsA

countOverlappingRangePairs : List RangePair -> Nat
countOverlappingRangePairs = \rangePairs ->
    List.countIf rangePairs rangesOverlap

rangesOverlap : RangePair -> Bool
rangesOverlap = \{ a, b } ->
    aOverlapsB = a.start <= b.start && b.start <= a.end
    bOverlapsA = b.start <= a.start && a.start <= b.end

    aOverlapsB || bOverlapsA

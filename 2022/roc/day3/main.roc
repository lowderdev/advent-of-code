app "day3"
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

inputPath : Path.Path
inputPath = Path.fromStr "part1.txt"

main : Program
main = Program.quick mainTask

mainTask : Task {} [] [Read [File], Write [Stderr, Stdout]]
mainTask =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        sacks = parse inputString

        itemTypes <- sacks |> List.mapTry determineItemType |> Task.fromResult |> Task.await
        p1 = sumPriorities itemTypes
        _ <- Stdout.line "Part1: \(p1)" |> Task.await

        groups = groupSacks sacks
        _ <- Stdout.line "groups" |> Task.await
        badgeTypes <- groups |> List.mapTry determineBadge |> Task.fromResult |> Task.await
        p2 = sumPriorities badgeTypes

        _ <- Stdout.line "Part1: \(p1)" |> Task.await
        Stdout.line "Part2: \(p2)"

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.succeed {}
            Err err ->
                when err is
                    FileReadErr _ _ -> Stderr.line "Error reading file"
                    FileReadUtf8Err _ _ -> Stderr.line "Error with path"
                    NotFound -> Stderr.line "No itemType found"
                    InvalidGroupErr -> Stderr.line "Invalid group"

parse : Str -> List Str
parse = \str ->
    str
    |> Str.trim
    |> Str.split "\n"

groupSacks : List Str -> List (List Str)
groupSacks = \sacks ->
    group = List.takeFirst sacks 3

    List.concat [group] (groupSacks (List.drop sacks 3))

determineItemType : Str -> Result Str [NotFound]
determineItemType = \sack ->
    items = Str.graphemes sack
    len = List.len items
    half = len // 2
    firstHalf = List.takeFirst items half
    secondHalf = List.drop items half

    List.findFirst firstHalf \elem -> List.contains secondHalf elem

determineBadge : List Str -> Result Str [InvalidGroupErr]
determineBadge = \group ->
    when group is
        [a, b, c] ->
            aItems = Str.graphemes a
            bItems = Str.graphemes b
            cItems = Str.graphemes c

            List.findFirst aItems \elem -> List.contains bItems elem && List.contains cItems elem

        _ -> Err InvalidGroupErr

sumPriorities : List Str -> Str
sumPriorities = \itemTypes ->
    itemTypes
    |> List.map scoreItem
    |> List.walk 0 Num.add
    |> Num.toStr

scoreItem : Str -> Nat
scoreItem = \letter ->
    # it was late and I was tired and lazy ok?!
    when letter is
        "a" -> 1
        "b" -> 2
        "c" -> 3
        "d" -> 4
        "e" -> 5
        "f" -> 6
        "g" -> 7
        "h" -> 8
        "i" -> 9
        "j" -> 10
        "k" -> 11
        "l" -> 12
        "m" -> 13
        "n" -> 14
        "o" -> 15
        "p" -> 16
        "q" -> 17
        "r" -> 18
        "s" -> 19
        "t" -> 20
        "u" -> 21
        "v" -> 22
        "w" -> 23
        "x" -> 24
        "y" -> 25
        "z" -> 26
        "A" -> 27
        "B" -> 28
        "C" -> 29
        "D" -> 30
        "E" -> 31
        "F" -> 32
        "G" -> 33
        "H" -> 34
        "I" -> 35
        "J" -> 36
        "K" -> 37
        "L" -> 38
        "M" -> 39
        "N" -> 40
        "O" -> 41
        "P" -> 42
        "Q" -> 43
        "R" -> 44
        "S" -> 45
        "T" -> 46
        "U" -> 47
        "V" -> 48
        "W" -> 49
        "X" -> 50
        "Y" -> 51
        "Z" -> 52
        _ -> 0

app "day8"
    packages { pf: "../../../../basic-cli/src/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

ParseState : { x : Nat, y : Nat, maxX : Nat, maxY : Nat, grid : TreeGrid }
TreeGrid : Dict Coords Nat
Coords : { x : Nat, y : Nat }

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        { grid } <- parse inputString |> Task.fromResult |> Task.await
        count = grid |> countVisibleTrees |> Num.toStr

        Stdout.line "Answer: \(count)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            InvalidNumStr | Overflow -> Stderr.line "Error parsing"

one : Nat
one = 1

two : Nat
two = 2

parse : Str -> Result ParseState [InvalidNumStr, Overflow]
parse = \inputString ->
    lines = inputString |> Str.trim |> Str.split "\n"

    firstLine <- lines |> List.first |> Result.try

    overX = firstLine
        |> Str.graphemes
        |> List.len

    maxX <- Num.subChecked overX 1 |> Result.try

    res2 =
        lines
        |> List.walkTry { x: 0, y: 0, grid: Dict.empty } \state, line ->
            res =
                line
                |> Str.graphemes
                |> List.walkTry state \{ x: x1, y: y1, grid: g1 }, height ->
                    h2 <- height |> Str.toNat |> Result.try
                    x2 <- Num.addChecked x1 1 |> Result.try
                    g2 = Dict.insert g1 { x: x2, y: y1 } h2

                    Ok { x: x2, y: y1, grid: g2 }

            when res is
                Ok { x: x3, y: y3, grid: g3 } ->
                    y4 <- Num.addChecked y3 one |> Result.try
                    Ok { x: 0, y: y4, grid: g3 }
                err -> err

    when res2 is
        Ok { x: newX, y: newY, grid: newGrid } ->
            maxY <- Num.subChecked newY two |> Result.try
            Ok { x: newX, y: newY, grid: newGrid, maxX, maxY }
        err -> err

countVisibleTrees : TreeGrid -> Nat
# countVisibleTrees = \grid ->

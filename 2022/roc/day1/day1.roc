app "day1"
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

main : Program
main = Program.noArgs mainTask

mainTask : Task ExitCode [] [Read [File], Write [Stderr, Stdout]]
mainTask =
    path = Path.fromStr "part1.txt"
    task =
        contents <- File.readUtf8 path |> Task.await
        parsed = parseInput contents
        maxNum = top3 parsed

        Stdout.line (Num.toStr maxNum)

    Task.attempt task \result ->
        when result is
            Ok {} ->
                Stdout.line "Success"
                |> Program.exit 0

            Err err ->
                msg =
                    when err is
                        FileReadErr _ _ -> "Error reading file"
                        FileReadUtf8Err _ _ -> "Error with path"

                Stderr.line msg
                |> Program.exit 1

parseInput : Str -> List (List U64)
parseInput = \contents ->
    Str.trim contents
    |> Str.split "\n\n"
    |> List.map \numLines ->
        Str.split numLines "\n"
        |> List.map \num ->
            Str.toU64 num |> Result.withDefault 0

# part1
# calculateMax : List (List U64) -> U64
# calculateMax = \list ->
#     List.map list sumNums
#     |> List.max
#     |> Result.withDefault 0
#
# part 2
top3 : List (List U64) -> U64
top3 = \list ->
    List.map list sumNums
    |> List.sortDesc
    |> List.takeFirst 3
    |> List.walk 0 Num.add

sumNums : List (Num a) -> Num a
sumNums = \nums ->
    List.walk nums 0 Num.add

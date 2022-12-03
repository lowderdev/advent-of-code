app "day1"
    packages { pf: "../roc_nightly-macos_11_x86_64-2022-11-23-0ac6fe7/examples/cli/cli-platform/main.roc" }
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
    path = Path.fromStr "day1.txt"
    task =
        contents <- File.readUtf8 path |> Task.await
        parsed = parseInput contents
        maxNum =
            List.map parsed sumNums
            |> List.max
            |> Result.withDefault 0

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

sumNums : List (Num a) -> Num a
sumNums = \nums ->
    List.walk nums 0 Num.add

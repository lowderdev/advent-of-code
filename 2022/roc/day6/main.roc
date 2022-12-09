app "day6"
    packages { pf: "../../../../basic-cli/src/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        lastCharPosition <- findMsg inputString |> Task.fromResult |> Task.await
        limit = Num.toStr lastCharPosition

        Stdout.line "Answer: \(limit)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            NoMsgFound -> Stderr.line "Parsing error"

findMsg : Str -> Result Nat [NoMsgFound]
findMsg = \input ->
    input
    |> Str.trim
    |> Str.graphemes
    |> searchLetters 4

searchLetters : List Str, Nat -> Result Nat [NoMsgFound]
searchLetters = \letters, limit ->
    { before } = List.split letters 4

    when before is
        [_, _, _, _] ->
            if allDifferent before then
                Ok limit
            else
                searchLetters (List.dropFirst letters) (limit + 1)

        _ -> Err NoMsgFound

allDifferent : List Str -> Bool
allDifferent = \letters ->
    state = { seen: Set.empty, unique: Bool.true }
    { unique: msgFound } = List.walkUntil letters state \{ seen, unique }, letter ->
        if Set.contains seen letter then
            Break { seen, unique: Bool.false }
        else
            Continue { seen: Set.insert seen letter, unique }

    msgFound

app "day7"
    packages { pf: "../../../../basic-cli/src/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Stderr,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

InputLine : [Cd Str, Ls, Back, DirLine Str, FileLine Nat]
DirState : { cwd : DirPath, dirs : DirListing }
DirListing : Dict DirPath Nat
DirPath : List Str

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        inputLines <- parse inputString |> Task.fromResult |> Task.await
        dirSizesNum = sizeDirectories inputLines
        dirSizes = Num.toStr dirSizesNum

        Stdout.line "Answer: \(dirSizes)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            InvalidInputLine -> Stderr.line "Error: InvalidInputLine"
            InvalidNumStr -> Stderr.line "Error: InvalidNumStr"

parse : Str -> Result (List InputLine) [InvalidInputLine, InvalidNumStr]
parse = \lines ->
    lines
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseInputLine

parseInputLine : Str -> Result InputLine [InvalidInputLine, InvalidNumStr]
parseInputLine = \line ->
    tokens = line |> Str.split " "

    when tokens is
        ["$", "cd", ".."] -> Ok Back
        ["$", "cd", cdDir] -> Ok (Cd cdDir)
        ["$", "ls"] -> Ok Ls
        ["dir", dirName] -> Ok (DirLine dirName)
        [sizeStr, _] ->
            size <- Str.toNat sizeStr |> Result.try
            Ok (FileLine size)

        _ -> Err InvalidInputLine

sizeDirectories : List InputLine -> Nat
sizeDirectories = \inputLines ->
    dirState = { cwd: [], dirs: Dict.empty }
    finalState = List.walk inputLines dirState traverseLines
    finalSize = Dict.walk finalState.dirs 0 \s, _, v ->
        if v < 100000 then
            s + v
        else
            s

    finalSize

traverseLines : DirState, InputLine -> DirState
traverseLines = \dirState, inputLine ->
    { cwd, dirs } = dirState

    when inputLine is
        Back -> { dirState & cwd: List.dropFirst cwd }
        Cd dirName -> { dirState & cwd: List.prepend cwd dirName }
        Ls -> dirState
        DirLine _ -> dirState
        FileLine fileSize ->
            updatedDirs = updateParentDirs dirs cwd fileSize

            { dirState & dirs: updatedDirs }

# Based on code from Luke!
# https://github.com/lukewilliamboswell/roc-things/blob/4e31df0f88e792e2e955dcb58c58605b5bfbb358/aoc-2022/day7.roc
updateParentDirs : DirListing, DirPath, Nat -> DirListing
updateParentDirs = \dirs, cwd, fileSize ->
    updatedDirs = Dict.update dirs cwd \possibleValue ->
        when possibleValue is
            Present size -> Present (size + fileSize)
            Missing -> Present fileSize

    when List.dropFirst cwd is
        [] -> dirs
        parentDir -> updateParentDirs updatedDirs parentDir fileSize

alterValue : [Present Bool, Missing] -> [Present Bool, Missing]
alterValue = \possibleValue ->
    when possibleValue is
        Missing -> Present Bool.false
        Present value -> if value then Missing else Present Bool.true

expect Dict.update Dict.empty "a" alterValue == Dict.single "a" Bool.false
expect Dict.update (Dict.single "a" Bool.false) "a" alterValue == Dict.single "a" Bool.true
expect Dict.update (Dict.single "a" Bool.true) "a" alterValue == Dict.empty

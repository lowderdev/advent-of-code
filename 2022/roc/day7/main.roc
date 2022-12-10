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

InputLine : [Cd Str, Ls, Back, DirLine Str, FileLine U64]
DirState : { cwd : DirPath, dirs : DirListing }
DirListing : Dict DirPath U64
DirPath : List Str

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        inputString <- inputPath |> File.readUtf8 |> Task.await
        inputLines <- parse inputString |> Task.fromResult |> Task.await
        dirs = sizeDirectories inputLines
        smallDirSizes = dirs |> sizeSmallDirectories |> Num.toStr

        # had to add a starting string or it didn't work
        totalDiskUsed <- Dict.get dirs ["/", "__ROOT__"] |> Task.fromResult |> Task.await
        freeSpace <- Num.subChecked 70000000 totalDiskUsed |> Task.fromResult |> Task.await
        spaceNeeded <- Num.subChecked 30000000 freeSpace |> Task.fromResult |> Task.await
        deleteDirSizes = findDirToDelete dirs spaceNeeded
        deleteDirSize = deleteDirSizes |> List.min |> Result.withDefault 0 |> Num.toStr

        Stdout.line "Answer: \(smallDirSizes), deleteDirSize: \(deleteDirSize)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            InvalidInputLine -> Stderr.line "Error: InvalidInputLine"
            InvalidNumStr -> Stderr.line "Error: InvalidNumStr"
            Overflow -> Stderr.line "Error: Overflow"
            KeyNotFound -> Stderr.line "Error: KeyNotFound"

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
            size <- Str.toU64 sizeStr |> Result.try
            Ok (FileLine size)

        _ -> Err InvalidInputLine

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
updateParentDirs : DirListing, DirPath, U64 -> DirListing
updateParentDirs = \dirs, cwd, fileSize ->
    updatedDirs = Dict.update dirs cwd \possibleValue ->
        when possibleValue is
            Present size -> Present (size + fileSize)
            Missing -> Present fileSize

    when List.dropFirst cwd is
        [] -> dirs
        parentDir -> updateParentDirs updatedDirs parentDir fileSize

sizeDirectories : List InputLine -> DirListing
sizeDirectories = \inputLines ->
    dirState = { cwd: ["__ROOT__"], dirs: Dict.empty }
    finalState = List.walk inputLines dirState traverseLines

    finalState.dirs

sizeSmallDirectories : DirListing -> U64
sizeSmallDirectories = \dirs ->
    Dict.walk dirs 0 \totalSize, _, dirSize ->
        if dirSize < 100000 then totalSize + dirSize else totalSize

findDirToDelete : DirListing, U64 -> List U64
findDirToDelete = \dirs, spaceNeeded ->
    Dict.walk dirs [] \sizes, _, dirSize ->
        if dirSize >= spaceNeeded then
            List.append sizes dirSize
        else
            sizes

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

TreeGrid : Dict Coords Tree
Coords : { x : U16, y : U16 }
Tree : { height : U16, visible : Bool }
BuildState : { x : U16, y : U16, grid : TreeGrid }
CountState : { grid : TreeGrid, maxX : U16, maxY : U16, tallest : U16, visibleCount : U16, x : U16, y : U16 }

inputPath : Path.Path
inputPath = Path.fromStr "input.txt"

main : Task {} []
main =
    task =
        _ <- Stdout.line "starting" |> Task.await
        inputString <- inputPath |> File.readUtf8 |> Task.await

        maxX <- inputString |> getMaxX |> Task.fromResult |> Task.await
        maxY <- inputString |> getMaxY |> Task.fromResult |> Task.await
        rows <- parse inputString |> Task.fromResult |> Task.await
        grid <- rows |> buildGrid |> Task.fromResult |> Task.await
        state = { grid, maxX, maxY, tallest: 0, visibleCount: 0, x: 0, y: 0 }
        { grid: gridLeft } <- state |> inspectFromLeft |> Task.fromResult |> Task.await
        { grid: gridTop } <- { state & grid: gridLeft } |> inspectFromTop |> Task.fromResult |> Task.await
        { grid: gridRight } <- { state & grid: gridTop } |> inspectFromRight |> Task.fromResult |> Task.await
        { grid: finalGrid } <- { state & grid: gridRight } |> inspectFromBottom |> Task.fromResult |> Task.await

        countNum <- (countVisible finalGrid) |> Task.fromResult |> Task.await

        # countNum <- countVisibleTrees grid maxX maxY |> Task.fromResult |> Task.await
        count = Num.toStr countNum

        Stdout.line "Answer: \(count)"

    Task.onFail task \err ->
        when err is
            FileReadErr _ _ -> Stderr.line "Error reading file"
            FileReadUtf8Err _ _ -> Stderr.line "Error reading file as utf8"
            InvalidNumStr -> Stderr.line "Error: InvalidNumStr"
            KeyNotFound -> Stderr.line "Error: KeyNotFound"
            ListWasEmpty -> Stderr.line "Error: ListWasEmpty"
            Overflow -> Stderr.line "Error: Overflow"

getMaxX : Str -> Result U16 [ListWasEmpty]
getMaxX = \input ->
    firstRow <- input
        |> Str.split "\n"
        |> List.first
        |> Result.try

    firstRow
    |> Str.graphemes
    |> List.len
    |> Num.toU16
    |> Num.subChecked 1

getMaxY : Str -> Result U16 [ListWasEmpty]
getMaxY = \input ->
    numRows =
        input
        |> Str.split "\n"
        |> List.len
        |> Num.toU16

    Num.subChecked numRows 1

parse : Str -> Result (List (List U16)) [InvalidNumStr]
parse = \inputString ->
    inputString
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseRow

parseRow : Str -> Result (List U16) [InvalidNumStr]
parseRow = \line ->
    line
    |> Str.graphemes
    |> List.mapTry Str.toU16

buildGrid : List (List U16) -> Result TreeGrid [Overflow]
buildGrid = \rows ->
    state = { grid: Dict.empty, x: 0, y: 0 }

    { grid } <- List.walkTry rows state buildRow |> Result.try
    Ok grid

buildRow : BuildState, List U16 -> Result BuildState [Overflow]
buildRow = \state, row ->
    { grid, y } <- List.walkTry row state insertHeight |> Result.try
    y2 <- Num.addChecked y 1 |> Result.try
    Ok { grid, x: 0, y: y2 }

insertHeight : BuildState, U16 -> Result BuildState [Overflow]
insertHeight = \{ grid, x, y }, height ->
    newGrid = Dict.insert grid { x, y } { height, visible: Bool.false }

    x2 <- Num.addChecked x 1 |> Result.try
    Ok { grid: newGrid, x: x2, y }

# countVisibleTrees : TreeGrid, U16, U16 -> Result U16 [Overflow]
# countVisibleTrees = \grid, maxX, maxY ->
#     state = { grid, maxX, maxY, tallest: 0, visibleCount: 0, x: 0, y: 0 }

#     { grid: gridLeft } <- state |> inspectFromLeft |> Result.try
#     { grid: gridTop } <- { state & grid: gridLeft } |> inspectFromTop |> Result.try
#     { grid: gridRight } <- { state & grid: gridTop } |> inspectFromRight |> Result.try
#     { grid: finalGrid } <- { state & grid: gridRight } |> inspectFromBottom |> Result.try

#     countVisible finalGrid

inspectFromLeft : CountState -> Result CountState [Overflow]
inspectFromLeft = \state ->
    { maxX, maxY, x, y } = state

    if y > maxY then
        Ok state
    else if x == 0 || y == 0 || y == maxY then
        newState <- markTreeVisible state |> Result.try
        nextX <- Num.addChecked x 1 |> Result.try
        inspectFromLeft { newState & x: nextX }
    else if x == maxX then
        newState <- markTreeVisible state |> Result.try
        nextY <- Num.addChecked y 1 |> Result.try
        inspectFromLeft { newState & x: 0, y: nextY }
    else
        newState <- checkTree state |> Result.try
        nextX <- Num.addChecked x 1 |> Result.try
        Ok { newState & x: nextX }

inspectFromTop : CountState -> Result CountState [Overflow]
inspectFromTop = \state ->
    { maxX, maxY, x, y } = state

    if x > maxX then
        Ok state
    else if x == 0 || y == 0 || x == maxX then
        newState <- markTreeVisible state |> Result.try
        nextY <- Num.addChecked y 1 |> Result.try
        inspectFromLeft { newState & y: nextY }
    else if y == maxY then
        newState <- markTreeVisible state |> Result.try
        nextX <- Num.addChecked x 1 |> Result.try
        inspectFromLeft { newState & x: nextX, y: 0 }
    else
        newState <- checkTree state |> Result.try
        nextY <- Num.addChecked y 1 |> Result.try
        Ok { newState & y: nextY }

inspectFromRight : CountState -> Result CountState [Overflow]
inspectFromRight = \state ->
    { maxX, maxY, x, y } = state

    if y > maxY then
        Ok state
    else if x == maxX || y == 0 || y == maxY then
        newState <- markTreeVisible state |> Result.try
        nextX <- Num.subChecked x 1 |> Result.try
        inspectFromLeft { newState & x: nextX }
    else if x == 0 then
        newState <- markTreeVisible state |> Result.try
        nextY <- Num.addChecked y 1 |> Result.try
        inspectFromLeft { newState & x: maxX, y: nextY }
    else
        newState <- checkTree state |> Result.try
        nextX <- Num.subChecked x 1 |> Result.try
        Ok { newState & x: nextX }

inspectFromBottom : CountState -> Result CountState [Overflow]
inspectFromBottom = \state ->
    { maxX, maxY, x, y } = state

    if x > maxX then
        Ok state
    else if x == 0 || y == maxY || x == maxX then
        newState <- markTreeVisible state |> Result.try
        nextY <- Num.subChecked y 1 |> Result.try
        inspectFromLeft { newState & y: nextY }
    else if y == 0 then
        newState <- markTreeVisible state |> Result.try
        nextX <- Num.addChecked x 1 |> Result.try
        inspectFromLeft { newState & x: nextX, y: maxY }
    else
        newState <- checkTree state |> Result.try
        nextY <- Num.subChecked y 1 |> Result.try
        Ok { newState & y: nextY }

markTreeVisible : CountState -> Result CountState [KeyNotFound, Overflow]
markTreeVisible = \state ->
    { grid, tallest, visibleCount, x, y } = state

    tree <- Dict.get grid { x, y } |> Result.try
    newTallest = if tallest >= tree.height then tallest else tree.height
    newGrid = Dict.insert grid { x, y } { tree & visible: Bool.true }

    newVisibleCount <- Num.addChecked visibleCount 1 |> Result.try
    Ok { state & grid: newGrid, tallest: newTallest, visibleCount: newVisibleCount }

checkTree : CountState -> Result CountState [KeyNotFound, Overflow]
checkTree = \state ->
    { grid, tallest, visibleCount, x, y } = state

    tree <- Dict.get grid { x, y } |> Result.try
    newTallest = if tallest >= tree.height then tallest else tree.height

    if tree.visible || tallest >= tree.height then
        Ok { state & tallest: newTallest }
    else
        newGrid = Dict.insert grid { x, y } { tree & visible: Bool.true }

        i <- Num.addChecked visibleCount 1 |> Result.try
        Ok { state & grid: newGrid, visibleCount: i }

countVisible : TreeGrid -> Result U16 [Overflow]
countVisible = \grid ->
    Dict.walk grid (Ok 0) \resN, _, { visible: v } ->
        n <- resN |> Result.try
        if v then
            Num.addChecked n 1
        else
            Ok n

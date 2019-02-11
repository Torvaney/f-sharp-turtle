module TurtleRunner

open System

type PenState =
    | Up
    | Down

type Colour =
    | Black
    | Red
    | Blue
    | Green

type TurnDirection =
    | Left
    | Right

type Angle =
    | Degrees of float
    | Radians of float

type Scalar = float

type Turtle = {
    xpos: Scalar
    ypos: Scalar
    angle: Angle
    penState: PenState
    colour: Colour
}

type Command =
    | Move of Scalar
    | Turn of TurnDirection * Angle
    | SetPen of PenState
    | SetColour of Colour

let getRadians (angle:Angle) :float =
    match angle with
    | Degrees angle -> angle * Math.PI/180.0
    | Radians angle -> angle

let turn initAngle direction turnAngle =
    let initRad = getRadians initAngle
    let turnRad = getRadians turnAngle
    match direction with
    | Left -> Radians (initRad - turnRad)
    | Right -> Radians (initRad + turnRad)

// Function to apply state changes in a Command to a Turtle
// Uses pattern matching to deconstruct the command into the different cases
let processCommand turtle command =
    match command with
    | Move distance ->
        let rad = getRadians turtle.angle
        {turtle with xpos = turtle.xpos + distance*Math.Sin(rad)
                     ypos = turtle.ypos + distance*Math.Cos(rad)}
    | Turn(direction, turnAngle) ->
        {turtle with angle = turn turtle.angle direction turnAngle}
    | SetPen state ->
        {turtle with penState = state}
    | SetColour colour ->
        {turtle with colour = colour}


// --- Uncomment this section to run a full test ---
// List of Commands to apply
let commands = [
    Move 20.0
    Turn(Left, Degrees 90.0)
    Move 20.0
    Turn(Right, Degrees 90.0)
    SetColour Red
    Move 20.0
    Turn(Right, Radians (Math.PI / 2.))
    SetPen Up
    Move 40.0
]
// Our initial Turtle instance
let turtle = {
    xpos = 0.0
    ypos = 0.0
    angle = Degrees 90.0
    penState = Down
    colour = Black
 }


// Apply all the commands to the Turtle in turn
let movedTurtle =
    commands
    |> List.fold (fun agg command -> processCommand agg command) turtle

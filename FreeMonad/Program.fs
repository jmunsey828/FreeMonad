// Learn more about F# at http://fsharp.org

open System

type Direction = 
    | North 
    | South
    | East
    | West 

type Rover = { 
    direction: Direction
    x: int
    y: int 
    }

type RoverInstruction<'a> =
    | Forward of Rover * (Rover -> 'a)
    | Backward of Rover * (Rover -> 'a) 
    | TurnLeft of Rover * (Rover -> 'a) 
    | TurnRight of Rover * (Rover -> 'a) 

type RoverProgram<'a> =
    | Free of RoverInstruction<RoverProgram<'a>>
    | Pure of 'a

let private mapI f = function 
    | Forward (r, next) -> Forward (r, next >> f)
    | Backward (r, next) -> Backward (r, next >> f)
    | TurnLeft (r, next) -> TurnLeft (r, next >> f)
    | TurnRight (r, next) -> TurnRight (r, next >> f)

let rec bind f = function 
    | Free i -> i |> mapI (bind f) |> Free 
    | Pure x -> f x 

type RoverBuilder()=
    member this.Bind(x, f) = bind f x
    member this.Return(x) = Pure x
    member this.ReturnFrom(x) = x
    member this.Zero() = Pure()

let rover = new RoverBuilder()

let forwardImp r =  
    match r.direction with 
    | North -> {r with y = (r.y + 1)} 
    | South -> {r with y = (r.y - 1)} 
    | East -> {r with x = (r.x + 1)} 
    | West -> {r with x = (r.x - 1)} 

let backwardImp r = 
    match r.direction with 
    | North -> {r with y = (r.y - 1)} 
    | South -> {r with y = (r.y + 1)} 
    | East -> {r with x = (r.x - 1)} 
    | West -> {r with x = (r.x + 1)} 

let turnLeftImp r =
    match r.direction with 
    | North -> {r with direction = West }
    | South -> {r with direction = East }
    | East -> {r with direction = North }
    | West -> {r with direction = South }

let turnRightImp r =
    match r.direction with 
    | North -> {r with direction = East } 
    | South -> {r with direction = West } 
    | East -> {r with direction = South } 
    | West -> {r with direction = North } 


let rec interpret = function 
    | Pure x -> x
    | Free(Forward (r, next)) -> r |> forwardImp |> next |> interpret
    | Free(Backward (r, next)) -> r |> backwardImp |> next |> interpret
    | Free(TurnLeft (r, next)) -> r |> turnLeftImp |> next |> interpret
    | Free(TurnRight (r, next)) -> r |> turnRightImp |> next |> interpret
        

[<EntryPoint>]
let main argv =
  
    let forward r = Free(Forward (r, Pure))
    let backward r = Free(Backward (r, Pure))
    let turnLeft r = Free(TurnLeft (r, Pure))
    let turnRight r = Free(TurnRight (r, Pure))

    let program r = rover {
        //let r = {x=0; y=0; direction=North}
        let! r = forward r // North (0, 1)
        let! r = turnLeft r // West (0, 1)
        let! r = forward r  // West (-1, 1)
        let! r = turnRight r // North (-1, 1)
        let! r = turnRight r // East (-1,1 )
        let! r = backward r // East (-2, 1)
        return r
        }


    let rover = {x=0; y=0; direction=North}
    //let rover1 = rover {x=0; y=0; direction=North}

    interpret (program rover) |> printfn "%A"

    0 // return an integer exit code

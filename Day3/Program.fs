// Learn more about F# at http://fsharp.org

open System
open System.IO

type Point = int * int 

let getWirePoints () =
    async {
        let! wireLines = File.ReadAllLinesAsync("input.txt") |> Async.AwaitTask
        let wirePoints = wireLines |> Array.map(fun s -> s.Split(','))
        return wirePoints
    }

let plotPoints(previousPoint : Point, instruction : string) =
    let amount = Int32.Parse (instruction.Substring 1)
    let direction = instruction.[0]
    let (previousX, previousY) = previousPoint
    let amountArray = [1..amount] |> List.rev
    match direction with
        | 'U' -> List.map(fun num -> Point(previousX, previousY + (num))) amountArray
        | 'D' -> List.map(fun num -> Point(previousX, previousY - (num))) amountArray
        | 'L' -> List.map(fun num -> Point(previousX - (num), previousY)) amountArray
        | 'R' -> List.map(fun num -> Point(previousX + (num), previousY)) amountArray
        | _ -> failwith "invalid direction"

let plotWire(wirePoints : string[]) = 
    Array.fold (fun points instruction -> List.append(plotPoints (points.Head, instruction)) points) [Point(0,0)] wirePoints

let findClosest(points1 : Point list, points2 : Point list) =
    let points1Set = Set.ofList points1
    let points2Set = Set.ofList points2
    let orderedDistance = Set.intersect points1Set points2Set
                            |> Set.filter(fun (x, y) -> x <> 0 && y <> 0)
                            |> Set.map(fun (x, y) -> Math.Abs(x) + Math.Abs(y))
                            |> Seq.sort
                            |> List.ofSeq
    orderedDistance.Head

let findIndexTotal(points1 : Point list, points2 : Point list, pointToFind: Point) =
    let points1Reverse = List.rev points1
    let points2Reverse = List.rev points2
    List.findIndex (fun (x2, y2) -> pointToFind = (x2, y2)) points1Reverse 
        + List.findIndex (fun (x2, y2) -> pointToFind = (x2, y2)) points2Reverse

let findClosestPartTwo(points1 : Point list, points2 : Point list) =
    let points1Set = Set.ofList points1
    let points2Set = Set.ofList points2
    let orderedDistance = Set.intersect points1Set points2Set
                            |> Set.filter(fun (x, y) -> x <> 0 && y <> 0)
                            |> Set.map(fun (x, y) -> findIndexTotal(points1, points2, (x,y)))
                            |> Seq.sort
                            |> List.ofSeq
    orderedDistance.Head

[<EntryPoint>]
let main argv =
    let wirePoints = getWirePoints() |> Async.RunSynchronously
    let plottedWires = wirePoints |> Seq.map plotWire
    let closestIntersection = findClosest (Seq.item(0) plottedWires, Seq.item(1) plottedWires)
    let closestIntersectionPartTwo = findClosestPartTwo (Seq.item(0) plottedWires, Seq.item(1) plottedWires)
    printfn "%i" closestIntersection
    printfn "%i" closestIntersectionPartTwo
    0 // return an integer exit code

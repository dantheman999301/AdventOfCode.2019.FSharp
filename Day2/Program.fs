// Learn more about F# at http://fsharp.org

open System
open System.IO

[<Literal>]
let AdditionOptCode = 1

[<Literal>]
let MultiplyOptCode = 2

[<Literal>]
let EndProgramOptCode = 99

let areAllInts (args: string[]) = 
    let tryParseAll = args |> Array.map Int32.TryParse
    (tryParseAll |> Array.exists (fun (a, _) -> a = false) = false, tryParseAll |> Array.map (fun (_, b) -> b)) 

let getOptCodes () =
    async {
        let! optCodesLine = File.ReadAllTextAsync("input.txt") |> Async.AwaitTask
        let optCodes = optCodesLine.Split(',')
        if optCodes |> Array.isEmpty then return None
        else 
            match areAllInts(optCodes) with 
            | (true, ints) ->
                return Some(ints)
            | (false, _) -> return None
    }

let executeAdditionOpCode(operation: int[], currentProgram: int[]) = 
    let additionResult = currentProgram.[operation.[1]] + currentProgram.[operation.[2]]
    Array.set currentProgram operation.[3] additionResult
    currentProgram

let executeMultiplyOpCode(operation: int[], currentProgram: int[]) = 
    let additionResult = currentProgram.[operation.[1]] * currentProgram.[operation.[2]]
    Array.set currentProgram operation.[3] additionResult
    currentProgram

let rec runOperandRec (index: int, currentProgram : int[]) = 
    match currentProgram.[index] with
        | EndProgramOptCode -> currentProgram
        | AdditionOptCode -> 
            let nextIndex = index + 4
            let modifiedProgram = executeAdditionOpCode(currentProgram.[index..index + 3], currentProgram)
            runOperandRec (nextIndex, modifiedProgram)
        | MultiplyOptCode -> 
            let nextIndex = index + 4
            let modifiedProgram = executeMultiplyOpCode(currentProgram.[index..index + 3], currentProgram)
            runOperandRec (nextIndex, modifiedProgram)
        | _ -> currentProgram
    
let resetToGravityHalt(brokenProgram: int[]) = 
    Array.set brokenProgram 1 12
    Array.set brokenProgram 2 2
    brokenProgram

[<EntryPoint>]
let main argv =
    
    match getOptCodes() |> Async.RunSynchronously with
        | Some program -> 
            let beforeGrav = resetToGravityHalt(program)
            let result = runOperandRec(0, beforeGrav)
            printfn "%s" (result |> Array.map string |> String.concat ",")
        | None ->
            printfn "Invalid input"
    0

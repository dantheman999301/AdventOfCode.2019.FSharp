
open System
open System.IO

let getFuelAmount (numberToDivide : decimal, divideBy : decimal) = 
    let roundedDownFuel = Math.Floor(numberToDivide / divideBy)
    roundedDownFuel - 2m

let areAllDecimals (args: string[]) = 
    let tryParseAll = args |> Array.map Decimal.TryParse
    (tryParseAll |> Array.exists (fun (a, _) -> a = false) = false, tryParseAll |> Array.map (fun (_, b) -> b)) 

let getMass () =
    async {
        let! lines = File.ReadAllLinesAsync("input.txt") |> Async.AwaitTask
        if lines |> Array.isEmpty then return None
        else 
            match areAllDecimals(lines) with 
            | (true, decimals) ->
                return Some(Array.fold (fun acc decimal -> acc + getFuelAmount(decimal, 3m)) 0m decimals)
            | (false, _) -> return None
    }
            

[<EntryPoint>]
let main argv =
    match getMass() |> Async.RunSynchronously with
    | Some mass -> printfn "Answer: %f" mass
    | None -> printfn "Args were invalid"   
    0
// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

let puzzleInput = "284639-748759"

let stringSplitNextChar s = Regex("""(.)\1*""").Matches(s) 
                            |> Seq.cast<Match> 
                            |> Seq.map (fun m -> m.Value)
                            |> Seq.filter (fun m -> m.Length > 1)
                            |> Seq.toArray

let filterNumbersContainingZero(numbers : (int * string)[]) = 
    Array.filter(fun (_, stringNum) -> String.exists (fun c -> c = '0') stringNum = false) numbers

let isAnyDuplicatePairs(charPairs : char[] seq) = 
    charPairs |> Seq.filter(fun charPair -> charPair.[0] = charPair.[1])
              |> Seq.length > 0

let filterIfNoPairedNumbers(numbers : (int * string)[]) = 
    numbers |> Seq.filter(fun (_, stringNum) -> Seq.windowed 2 stringNum |> isAnyDuplicatePairs)
            |> Seq.toArray

let rec charSequenceIncreasing(index: int, digits : int[]) = 
    if index = digits.Length - 1 then true
    else
        let numToCompare = digits.[index..index+1]
        if numToCompare.[1] >= numToCompare.[0] then charSequenceIncreasing(index + 1, digits)
        else false

let filterNonIncreasingNumbers(numbers : (int * string)[]) = 
    numbers |> (Seq.filter(fun (_, stringNum) -> stringNum 
                                                |> Seq.toArray 
                                                |> Array.map(fun i -> int(Char.GetNumericValue(i))) 
                                                |> (fun digits -> charSequenceIncreasing(0, digits))))
                                                |> Seq.toArray 


let isValidPasswordPartTwo(passwordChunks : string[]) = 
    match Array.tryFind (fun chunk -> String.length chunk = 2) passwordChunks with // If any are two, pass the test
    | Some _ -> true
    | None -> false
        

let filterIfNoPairedPartTwo(numbers : (int * string)[]) =
    numbers |> Array.filter(fun (_, stringNum) -> stringSplitNextChar stringNum |> isValidPasswordPartTwo) 

[<EntryPoint>]
let main argv =
    let inputSplit = puzzleInput.Split('-')
    let lowerbound = Int32.Parse inputSplit.[0]
    let upperbound = Int32.Parse inputSplit.[1]

    let allPossibleNumbers = [| for i in lowerbound .. upperbound -> (i, string i) |]
    let result = allPossibleNumbers
                 |> filterNumbersContainingZero
                 |> filterIfNoPairedNumbers
                 |> filterNonIncreasingNumbers
                 |> filterIfNoPairedPartTwo
    printfn "Possible combinations %i" result.Length
    0 // return an integer exit code

// Learn more about F# at http://fsharp.org

open System

let puzzleInput = "284639-748759"

let filterNumbersContainingZero(numbers : (int * string)[]) = 
    Array.filter(fun (_, stringNum) -> String.exists (fun c -> c = '0') stringNum = false) numbers

let isAnyDuplicatePairs(charPairs : char[] seq) = 
    charPairs |> Seq.filter(fun charPair -> charPair.[0] = charPair.[1])
              |> Seq.length > 0

let filterNoPairedNumbers(numbers : (int * string)[]) = 
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
    

[<EntryPoint>]
let main argv =
    let inputSplit = puzzleInput.Split('-')
    let lowerbound = Int32.Parse inputSplit.[0]
    let upperbound = Int32.Parse inputSplit.[1]

    let allPossibleNumbers = [| for i in lowerbound .. upperbound -> (i, string i) |]
    let result = allPossibleNumbers
                 |> filterNumbersContainingZero
                 |> filterNoPairedNumbers
                 |> filterNonIncreasingNumbers
                 |> Seq.toArray
    printfn "Possible combinations %i" result.Length
    0 // return an integer exit code

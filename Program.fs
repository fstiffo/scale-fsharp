// Learn more about F# at http://fsharp.org
open Scale
open UI

[<EntryPoint>]
let main argv =
    let mutable s = createStato @"stato.xml"
    Scale.stato <- s
    startApp
    printfn "%A" s
    printfn "%d" (cassa s)
    printfn "%d" (tesoretto s)
    printfn "%d %d %d %d" (quote s Michela) (quote s Gerardo) (quote s Elena)
        (quote s Giulia)
    printfn "%d" (prestito s)
    0 // return an integer exit code

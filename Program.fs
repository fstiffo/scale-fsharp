// Learn more about F# at http://fsharp.org
open Scale
open UI

[<EntryPoint>]
let main argv =
    createStato()
    startApp()
    printfn "%A" Scale.stato
    printfn "%d" (cassa Scale.stato)
    printfn "%d" (tesoretto Scale.stato)
    printfn "%d %d %d %d" (quote Scale.stato Michela)
        (quote Scale.stato Gerardo) (quote Scale.stato Elena)
        (quote Scale.stato Giulia)
    printfn "%d" (prestito Scale.stato)
    0 // return an integer exit code

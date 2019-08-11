// Learn more about F# at http://fsharp.org
open System
open Terminal.Gui
open NStack
open Scale


let ustr (x:string) = ustring.Make(x)

let stop = Action Application.RequestStop

let newFile() =
    let ok = Button (ustr "Ok", true, Clicked=stop)
    let cancel = Button (ustr "Cancel", Clicked=stop)
    let d = Dialog (ustr ("New File"), 50, 20, ok, cancel)
    Application.Run (d)

let quit() =
    if MessageBox.Query (50, 7, "Quit demo", "Are you sure you want to quit the demo?", "Yes", "No") = 0 then
       Application.Top.Running <- false

let buildMenu() =
    MenuBar ([|
        MenuBarItem (ustr ("File"), 
            [| MenuItem(ustr("_New"), "Creates a new file", System.Action newFile);
               MenuItem(ustr("_Quit"), null, System.Action quit)
             |])|])

[<EntryPoint>]
let main argv =

    Application.Init ()
    let top = Application.Top
    let win = Window (ustr "Hello", X=Pos.op_Implicit(0), Y=Pos.op_Implicit(1), Width=Dim.Fill(), Height=Dim.Fill())
    top.Add (buildMenu())
    top.Add (win)
    Application.Run ()
    let s = createStato @"stato.xml"
    printfn "%A" s
    printfn "%d" (cassa s)
    printfn "%d" (tesoretto s)
    printfn "%d %d %d %d" (quote s Michela) (quote s Gerardo) (quote s Elena) (quote s Giulia)
    printfn "%d" (prestito s)
    0 // return an integer exit code
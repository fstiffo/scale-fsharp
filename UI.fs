module UI

open System
open System.Collections
open Terminal.Gui
open NStack
open Scale

let ustr (x : string) = ustring.Make(x)
let stop = Action Application.RequestStop

let newFile() =
    let ok = Button(ustr "Ok", true, Clicked = stop)
    let cancel = Button(ustr "Cancel", Clicked = stop)
    let d = Dialog(ustr ("New File"), 50, 20, ok, cancel)
    Application.Run(d)

let quit() =
    if MessageBox.Query
           (50, 7, "Quit demo", "Are you sure you want to quit the demo?", "Yes",
            "No") = 0 then Application.Top.Running <- false

let buildMenu() =
    MenuBar
        ([| MenuBarItem
                (ustr ("_File"),
                 [| MenuItem
                        (ustr ("_New"), "Creates a new file",
                         System.Action newFile)
                    MenuItem(ustr ("_Quit"), null, System.Action quit) |]) |])

let buildMovimentiList (s : Stato) =
    let ms = s.movimenti
    ()

let buildScrollView (s : Stato) =
    let l = (s.movimenti |> List.map MovimentoToString) |> List.toArray :> IList
    let listView = new ListView(Rect(0, 0, 43, 8), l)
    let frame = FrameView(Rect(60, 2, 45, 10), ustr "Movimenti")
    frame.Add(listView)
    frame :> View

let startApp (s : Stato) =
    Application.Init()
    // Colors.Base.Focus <- Attribute.Make(Color.BrightGreen, Color.Brown)
    let top = Application.Top
    let win =
        Window
            (ustr "Hello", X = Pos.op_Implicit (0), Y = Pos.op_Implicit (1),
             Width = Dim.Fill(), Height = Dim.Fill())
    win.Add(buildScrollView (s))
    top.Add(buildMenu())
    top.Add(win)
    Application.Run()

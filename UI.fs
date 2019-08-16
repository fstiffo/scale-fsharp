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

let movimentiProcessKey (lw : ListView) (kb : KeyEvent) =
    match (char kb.KeyValue) with
    | 'd' ->
        quit()
        true
    | _ -> lw.SuperView.ProcessHotKey(kb)

let buildMovimentiView =
    let s = Scale.stato

    let l =
        s.movimenti
        |> List.map ((MovimentoToString s) >> ustr)
        //|> List.map ustr
        |> List.toArray :> IList

    let listView =
        { new ListView(Rect(0, 0, 53, 18), l) with
              member x.ProcessColdKey(keyEvent : KeyEvent) =
                  (movimentiProcessKey x keyEvent) }

    listView.TopItem <- if (l.Count - 19) > 0 then l.Count - 19
                        else 1
    let frame = FrameView(Rect(60, 0, 55, 20), ustr "Movimenti")
    frame.Add(listView)
    frame :> View

let startApp =
    Application.Init()
    // Colors.Base.Focus <- Attribute.Make(Color.BrightGreen, Color.Brown)
    let top = Application.Top
    let win =
        Window
            (ustr "SCALE - v. 0.1", X = Pos.op_Implicit (0),
             Y = Pos.op_Implicit (1), Width = Dim.Fill(), Height = Dim.Fill())
    win.Add(buildMovimentiView)
    top.Add(buildMenu())
    top.Add(win)
    Application.Run()

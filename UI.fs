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

let newMovimento() : Option<Movimento> =
    let importo = TextField(ustr "0")

    let addMovimento() =
        let m =
            Prestito(try
                         int (importo.Text.ToString())
                     with _ -> 0)
        ignore <| MessageBox.ErrorQuery(10, 10, "Debug", string m, "Ok")

    // Application.RequestStop()
    let ok = Button(ustr "Ok", true, Clicked = Action addMovimento)
    let d = Dialog(ustr "Aggiungi movimento", 50, 20, ok)
    d.Add(importo)
    Application.Run(d)
    None

let movimentiIList() : IList =
    Scale.stato.movimenti
    |> List.map ((MovimentoToString Scale.stato) >> ustr)
    //|> List.map ustr
    |> List.toArray :> IList

let movimentiProcessKey (lw : ListView) (kb : KeyEvent) =
    match (char kb.KeyValue) with
    | 'e' when Scale.stato.movimenti.Length > 0 ->
        let selected = lw.SelectedItem
        let movimentoStr =
            MovimentoToString Scale.stato (Scale.stato.movimenti.Item selected)
        if MessageBox.Query
               (60, 9, "Elimina movimento",
                "Sei sicuro di voler eliminare:\n\n" + movimentoStr + " ?", "No",
                "Yes") <> 0 then
            Scale.deleteMovimento Scale.stato lw.SelectedItem
            |> Scale.updateStato
            lw.SetSource(movimentiIList())
            let len = Scale.stato.movimenti.Length
            if len > 0 then
                lw.SelectedItem <- if selected < len then selected
                                   else len - 1
            lw.SetNeedsDisplay()
        true
    | '\010' -> // RETURN key
        true
    | 'a' ->
        ignore <| newMovimento()
        true
    | _ -> lw.SuperView.ProcessHotKey(kb)

let buildMovimentiView() =
    let listView =
        { new ListView(Rect(0, 0, 53, 17), movimentiIList()) with
              member x.ProcessColdKey(keyEvent : KeyEvent) =
                  (movimentiProcessKey x keyEvent) }

    let frame = FrameView(Rect(60, 0, 55, 21), ustr "Movimenti")
    let cs = Colors.Menu
    // (!cs).Normal := Attribute.Make(Color.White, Color.BrightRed)
    let label =
        new Label(ustr "     [A] Aggiungi  [E] Elimina  [RETURN] Modifica    ",
                  Y = Pos.Bottom(listView) + Pos.At(1),
                  TextColor = Attribute.Make(Color.White, Color.Red))
    frame.Add(listView, label)
    frame :> View

let startApp() =
    Application.Init()
    let a = Application.Driver
    // Colors.Base.Focus <- Attribute.Make(Color.BrightGreen, Color.Brown)
    let top = Application.Top
    let win =
        Window
            (ustr "SCALE - v. 0.1", X = Pos.op_Implicit (0),
             Y = Pos.op_Implicit (1), Width = Dim.Fill(), Height = Dim.Fill())
    win.Add(buildMovimentiView())
    top.Add(buildMenu())
    top.Add(win)
    Application.Run()
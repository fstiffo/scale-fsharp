module UI

open System
open System.Collections
open System.Globalization
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

let delMovimento selected =
    let movimentoStr =
        MovimentoToString Scale.stato (Scale.stato.movimenti.Item selected)
    if MessageBox.Query
           (60, 9, "Elimina movimento",
            "Sei sicuro di voler eliminare:\n\n" + movimentoStr + " ?", "No",
            "Yes") <> 0 then
        Scale.deleteMovimento Scale.stato selected |> Scale.updateStato

let newMovimento() =
    let today = DateTime.Today
    let y, m, d = today.Year, today.Month, today.Day
    let dataLbl = Label(1, 1, ustr "Data:")
    let yearTxtFld = TextField(7, 1, 5, ustr (string y))
    let stroke1Lbl = Label(12, 1, ustr "/")
    let monthTxtFld = TextField(13, 1, 3, ustr (string m))
    let stroke2Lbl = Label(16, 1, ustr "/")
    let dayTxtFld = TextField(17, 1, 3, ustr (string d))
    let movimentoRdoGrp =
        RadioGroup
            (1, 3,
             [| "_1 Pagamento scale"; "_2 Versamento quote"; "_3 Prestito";
                "_4 Restituzione"; "_5 Altro versamento"; "_6 Altra spesa" |])
    let condominoLbl = Label(26, 3, ustr "Condomino:")
    let condominoRdoGrp =
        RadioGroup(26, 5, [| "_Michela"; "_Gerardo"; "_Elena"; "_Silvia" |])
    let importoLbl = Label(1, 10, ustr "Importo:")
    let importoTxtFld = TextField(10, 10, 6, ustr "0")
    let causaleLbl = Label(1, 12, ustr "Causale:")
    let causaleTxtFld = TextField(10, 12, 24, ustr "")

    let addMovimento() =
        let mutable importoErr = false
        let mutable dataErr = false

        let importo =
            try
                int (importoTxtFld.Text.ToString())
            with _ ->
                importoErr <- true
                -1

        let data =
            try
                DateTime.Parse
                    (dayTxtFld.Text.ToString() + "/"
                     + monthTxtFld.Text.ToString() + "/"
                     + yearTxtFld.Text.ToString(),
                     CultureInfo.CreateSpecificCulture("it-IT"))
            with _ ->
                dataErr <- true
                DateTime.Today

        if importo < 0 then importoErr <- true
        if importoErr || dataErr then
            ignore <| MessageBox.ErrorQuery(30, 10, "ERRORE",
                                            "Errore di formato in: \n"
                                            + (if importoErr then
                                                   "     | IMPORTO |  \n"
                                               else "")
                                            + (if dataErr then
                                                   "     |  DATA   |"
                                               else ""), "Ok")
        else
            let condomino =
                match condominoRdoGrp.Selected with
                | 0 -> Michela
                | 1 -> Gerardo
                | 2 -> Elena
                | 3 -> Giulia

            let operazione =
                match movimentoRdoGrp.Selected with
                | 0 -> PagamentoScale
                | 1 -> VersamentoQuote(condomino, importo)
                | 2 -> Prestito importo
                | 3 -> Restituzione importo
                | 4 -> AltroVersamento(causaleTxtFld.Text.ToString(), importo)
                | 5 -> AltraSpesa(causaleTxtFld.Text.ToString(), importo)

            let movimento = (dateTimeToData (data), operazione)
            Scale.addMovimento Scale.stato movimento |> Scale.updateStato
            Application.RequestStop()

    // ignore <| MessageBox.ErrorQuery(10, 10, "Debug", string data, "Ok")
    // Application.RequestStop()
    let ok = Button(ustr "Ok", true, Clicked = Action addMovimento)
    let annulla = Button(ustr "Annulla", false, Clicked = stop)
    let d = Dialog(ustr "Aggiungi movimento", 50, 19, ok, annulla)
    d.Add
        (dataLbl, yearTxtFld, stroke1Lbl, monthTxtFld, stroke2Lbl, dayTxtFld,
         movimentoRdoGrp, condominoLbl, condominoRdoGrp, importoLbl,
         importoTxtFld, causaleLbl, causaleTxtFld)
    Application.Run(d)

let movimentiIList() : IList =
    Scale.stato.movimenti
    |> List.map ((MovimentoToString Scale.stato) >> ustr)
    //|> List.map ustr
    |> List.toArray :> IList

let movimentiProcessKey (lw : ListView) (kb : KeyEvent) =
    match (char kb.KeyValue) with
    | 'e' when Scale.stato.movimenti.Length > 0 ->
        let selected = lw.SelectedItem
        delMovimento (selected)
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
        newMovimento()
        lw.SetSource(movimentiIList())
        lw.SetNeedsDisplay()
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
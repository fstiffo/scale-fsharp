module UI

open System
open System.Collections
open System.Globalization
open Terminal.Gui
open NStack
open Scale

let ustr (x : string) = ustring.Make(x)
let stop = Action Application.RequestStop

let quit() =
    if MessageBox.Query
           (50, 7, "Esci da SCALE",
            "Sei sicuro di volere chiudere l'applicazione?", "Ok", "Annulla") = 0 then
        Application.Top.Running <- false

let buildMenu() =
    MenuBar
        ([| MenuBarItem
                (ustr ("_File"),
                 [| MenuItem(ustr ("Esci"), null, System.Action quit) |]) |])

let buildRissuntoView() =
    let buildQuoteLbl text condomino y =
        let quote = quote Scale.stato condomino
        new Label(ustr <| text + (sprintf "% 4i" <| quote), X = Pos.At(1),
                  Y = Pos.At(y),
                  TextColor = if quote < 0 then
                                  Attribute.Make(Color.White, Color.Red)
                              else Attribute.Make(Color.White, Color.Green))

    let michelaLbl = buildQuoteLbl "Michela:     " Michela 1
    let gerardoLbl = buildQuoteLbl "Gerardo:     " Gerardo 3
    let elenaLbl = buildQuoteLbl "Elena:       " Elena 5
    let giuliaLbl = buildQuoteLbl "Giulia:      " Giulia 7
    let situazioneQuoteFrm =
        FrameView(Rect(1, 0, 39, 11), ustr "Situazione Quote")
    situazioneQuoteFrm.Add(michelaLbl, gerardoLbl, elenaLbl, giuliaLbl)
    let x0, y0 = Pos.Left(situazioneQuoteFrm), Pos.Bottom(situazioneQuoteFrm)
    let cassaLbl =
        new Label(ustr
                  <| "Cassa:     " + (sprintf "% 4i EUR" <| cassa Scale.stato),
                  X = x0, Y = y0 + Pos.At(1))
    let tesorettoLbl =
        new Label(ustr
                  <| "Tesoretto: "
                     + (sprintf "% 4i EUR" <| tesoretto Scale.stato), X = x0,
                  Y = y0 + Pos.At(3))
    let prestito = Scale.prestito Scale.stato

    let prestitoLbl =
        new Label(ustr <| "Prestito:  " + (sprintf "% 4i EUR" <| prestito),
                  X = x0, Y = y0 + Pos.At(5),
                  TextColor = if prestito > 0 then
                                  Attribute.Make(Color.White, Color.BrightRed)
                              else
                                  Attribute.Make(Color.White, Color.BrightGreen))

    let frame =
        FrameView
            (Rect(1, 0, 43, 20),
             ustr <| "Riassunto al " + DateTime.Today.ToShortDateString())
    frame.Add(situazioneQuoteFrm, cassaLbl, tesorettoLbl, prestitoLbl)
    frame :> View

let updateRiassunto() =
    let riassContentView =
        Application.Top.Subviews.Item(1).Subviews.Item(0).Subviews.Item(1)
                   .Subviews.Item(0)
    let situazQuoteContentView =
        riassContentView.Subviews.Item(0).Subviews.Item(0)
    let michelaLbl : Label = downcast situazQuoteContentView.Subviews.Item(0)
    let gerardoLbl : Label = downcast situazQuoteContentView.Subviews.Item(1)
    let elenaLbl : Label = downcast situazQuoteContentView.Subviews.Item(2)
    let giuliaLbl : Label = downcast situazQuoteContentView.Subviews.Item(3)
    let cassaLbl : Label = downcast riassContentView.Subviews.Item(1)
    let tesorettoLbl : Label = downcast riassContentView.Subviews.Item(2)
    let prestitoLbl : Label = downcast riassContentView.Subviews.Item(3)

    let updateQuoteLbl (lbl : Label) text condomino =
        let quote = quote Scale.stato condomino
        lbl.Text <- ustr <| text + (sprintf "% 4i" <| quote)
        lbl.TextColor <- if quote < 0 then
                             Attribute.Make(Color.White, Color.Red)
                         else Attribute.Make(Color.White, Color.Green)
    updateQuoteLbl michelaLbl "Michela:     " Michela
    updateQuoteLbl gerardoLbl "Gerardo:     " Gerardo
    updateQuoteLbl elenaLbl "Elena:       " Elena
    updateQuoteLbl giuliaLbl "Giulia:      " Giulia
    cassaLbl.Text <- ustr
                     <| "Cassa:     "
                        + (sprintf "% 4i EUR" <| cassa Scale.stato)
    tesorettoLbl.Text <- ustr
                         <| "Tesoretto: "
                            + (sprintf "% 4i EUR" <| tesoretto Scale.stato)
    let prestito = Scale.prestito Scale.stato
    prestitoLbl.Text <- ustr <| "Prestito:  " + (sprintf "% 4i EUR" <| prestito)
    prestitoLbl.TextColor <- if prestito > 0 then
                                 Attribute.Make(Color.White, Color.BrightRed)
                             else Attribute.Make(Color.White, Color.BrightGreen)

// Application.Run()
let delMovimento selected =
    let riassView =
        Application.Top.Subviews.Item(1).Subviews.Item(0).Subviews.Item(1)
    riassView.SetNeedsDisplay()
    let movimentoStr =
        MovimentoToString Scale.stato (Scale.stato.movimenti.Item selected)
    if MessageBox.Query
           (70, 10, "Elimina movimento",
            "\n Sei sicuro di voler eliminare:\n\n " + movimentoStr + " ?",
            "Annulla", "Ok") = 1 then
        Scale.deleteMovimento Scale.stato selected |> Scale.updateStato
        updateRiassunto()

//refreshRiassunto()
let modMovimento (selected : Option<int>) =
    let today = DateTime.Today
    let y, m, d = today.Year, today.Month, today.Day
    let dataLbl = Label(1, 1, ustr "Data:")
    let dayTxtFld = TextField(7, 1, 3, ustr (string d))
    let stroke1Lbl = Label(10, 1, ustr "/")
    let monthTxtFld = TextField(11, 1, 3, ustr (string m))
    let stroke2Lbl = Label(14, 1, ustr "/")
    let yearTxtFld = TextField(15, 1, 5, ustr (string y))
    let movimentoRdoGrp =
        RadioGroup
            (1, 3,
             [| "_1 Pagamento scale"; "_2 Versamento quote"; "_3 Prestito";
                "_4 Restituzione"; "_5 Altro versamento"; "_6 Altra spesa" |])
    let condominoLbl = Label(26, 3, ustr "Condomino:")
    let condominoRdoGrp =
        RadioGroup(26, 5, [| "_Michela"; "_Gerardo"; "_Elena"; "G_iulia" |])
    let importoLbl = Label(1, 10, ustr "Importo:")
    let importoTxtFld = TextField(10, 10, 6, ustr "0")
    let causaleLbl = Label(1, 12, ustr "Causale:")
    let causaleTxtFld = TextField(10, 12, 24, ustr "")
    match selected with
    | Some i ->
        let Data(y, m, d), op = Scale.stato.movimenti.Item(i)
        yearTxtFld.Text <- ustr <| string y
        monthTxtFld.Text <- ustr <| string m
        dayTxtFld.Text <- ustr <| string d
        match op with
        | PagamentoScale -> movimentoRdoGrp.Selected <- 0
        | VersamentoQuote(condomino, importo) ->
            movimentoRdoGrp.Selected <- 1
            importoTxtFld.Text <- ustr <| string importo
            condominoRdoGrp.Selected <- match condomino with
                                        | Michela -> 0
                                        | Gerardo -> 1
                                        | Elena -> 2
                                        | Giulia -> 3
        | Prestito importo ->
            movimentoRdoGrp.Selected <- 2
            importoTxtFld.Text <- ustr <| string importo
        | Restituzione importo ->
            movimentoRdoGrp.Selected <- 3
            importoTxtFld.Text <- ustr <| string importo
        | AltroVersamento(causale, importo) ->
            movimentoRdoGrp.Selected <- 4
            importoTxtFld.Text <- ustr <| string importo
            causaleTxtFld.Text <- ustr causale
        | AltraSpesa(causale, importo) ->
            movimentoRdoGrp.Selected <- 5
            importoTxtFld.Text <- ustr <| string importo
            causaleTxtFld.Text <- ustr causale
    | None -> ()
    // Core action associated to the OK  button
    let addOrReplaceMovimento (selected : Option<int>) (_ : unit) =
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
            match selected with
            | Some i ->
                Scale.replaceMovimento Scale.stato i movimento
                |> Scale.updateStato
            | None ->
                Scale.addMovimento Scale.stato movimento |> Scale.updateStato
            Application.RequestStop()
            updateRiassunto()

    let ok =
        Button
            (ustr "Ok", (selected = None),
             Clicked = Action(addOrReplaceMovimento selected))
    let annulla = Button(ustr "Annulla", (selected <> None), Clicked = stop)

    let d =
        Dialog(ustr <| (if selected = None then "Aggiungi movimento"
                        else "Modifica movimento"), 50, 19, ok, annulla)
    d.Add
        (dataLbl, yearTxtFld, stroke1Lbl, monthTxtFld, stroke2Lbl, dayTxtFld,
         movimentoRdoGrp, condominoLbl, condominoRdoGrp, importoLbl,
         importoTxtFld, causaleLbl, causaleTxtFld)
    dayTxtFld.EnsureFocus()
    Application.Run(d)

let movimentiIList() : IList =
    Scale.stato.movimenti
    |> List.map ((MovimentoToString Scale.stato) >> ustr)
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
        let selected = lw.SelectedItem
        modMovimento (Some selected)
        lw.SetSource(movimentiIList())
        lw.SelectedItem <- selected
        lw.SetNeedsDisplay()
        true
    | 'a' ->
        modMovimento (None)
        lw.SetSource(movimentiIList())
        lw.SetNeedsDisplay()
        true
    | _ -> lw.SuperView.ProcessHotKey(kb)

let buildMovimentiView() =
    let listView =
        { new ListView(Rect(1, 0, 64, 16), movimentiIList()) with
              member x.ProcessColdKey(keyEvent : KeyEvent) =
                  (movimentiProcessKey x keyEvent) }

    let frame = FrameView(Rect(48, 0, 68, 20), ustr "Movimenti")
    let label =
        new Label(ustr
                      "        [A] Aggiungi    [E] Elimina    [RETURN] Modifica        ",
                  X = Pos.At(1), Y = Pos.Bottom(listView) + Pos.At(1),
                  TextColor = Attribute.Make(Color.White, Color.Magenta))
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
    let rv = buildRissuntoView()
    win.Add(buildMovimentiView(), buildRissuntoView())
    top.Add(buildMenu())
    top.Add(win)
    Application.Run()
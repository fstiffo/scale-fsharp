module Scale

open System
open System.IO
open System.Runtime.Serialization.Formatters.Soap
open Itenso.TimePeriod

let fileName = @"stato_scale.xml"

[<Serializable>]
type Condomino =
    | Michela
    | Gerardo
    | Elena
    | Giulia

[<Serializable>]
type Operazione =
    | VersamentoQuote of Condomino * int
    | PagamentoScale
    | AltraSpesa of string * int
    | AltroVersamento of string * int
    | Prestito of int
    | Restituzione of int

[<Serializable>]
type Data = Data of int * int * int

[<Serializable>]
type Movimento = Data * Operazione

[<Serializable>]
type Param =
    { costoScale : int
      numPulizieMese : int
      quotaMensile : int }

[<Serializable>]
type Attuale = Data * Param

[<Serializable>]
type Stato =
    { tempoZero : Data
      attuale : Attuale
      condomini : Condomino list
      movimenti : Movimento list }

let mutable stato =
    { tempoZero = Data(0, 0, 0)
      attuale =
          (Data(0, 0, 0),
           { costoScale = 0
             numPulizieMese = 0
             quotaMensile = 0 })
      condomini = []
      movimenti = [] }

let createStato() =
    if (File.Exists(fileName)) then
        let sfStato = new SoapFormatter()
        let fsStato = new FileStream(fileName, FileMode.Open)
        let s = sfStato.Deserialize(fsStato) :?> Stato
        stato <- s
        ()
    else
        let t_0 = Data(2019, 7, 1)

        let s : Stato =
            { tempoZero = t_0
              attuale =
                  (t_0,
                   { costoScale = 20
                     numPulizieMese = 2
                     quotaMensile = 12 })
              condomini = [ Michela; Gerardo; Elena; Giulia ]
              movimenti =
                  [ (t_0, (AltroVersamento("Appianamento", 333)))
                    (t_0, VersamentoQuote(Michela, 74))
                    (t_0, VersamentoQuote(Gerardo, 78))
                    (t_0, VersamentoQuote(Elena, 48))
                    (Data(2019, 7, 22), Prestito 500)
                    (Data(2019, 7, 11), PagamentoScale)
                    (Data(2019, 8, 14), Restituzione 200)
                    (Data(2019, 8, 14), PagamentoScale) ] }

        let sfStato = new SoapFormatter()
        use fsStato = new FileStream(fileName, FileMode.Create)
        sfStato.Serialize(fsStato, s)
        stato <- s
        ()

let updateStato s =
    let sfStato = new SoapFormatter()
    use fsbStato = new FileStream(fileName + ".bak", FileMode.Create)
    sfStato.Serialize(fsbStato, stato)
    use fsStato = new FileStream(fileName, FileMode.Create)
    sfStato.Serialize(fsStato, s)
    stato <- s

let addMovimento (s : Stato) (m : Movimento) : Stato =
    { s with movimenti = s.movimenti @ [ m ] }

let deleteMovimento (s : Stato) (i : int) : Stato =
    let deleted =
        let first, second = List.splitAt i s.movimenti
        first @ second.Tail
    { s with movimenti = deleted }

let replaceMovimento (s : Stato) (i : int) (m : Movimento) : Stato =
    let replaced =
        let first, second = List.splitAt i s.movimenti
        first @ m :: second.Tail
    { s with movimenti = replaced }

let modifyMovimento (s : Stato) (i : int) (m : Movimento) = s

let private contabile (s : Stato) ((_, op) : Movimento) : int =
    match op with
    | VersamentoQuote(_, i) -> i
    | PagamentoScale -> -(snd s.attuale).costoScale
    | AltraSpesa(_, i) -> -i
    | AltroVersamento(_, i) -> i
    | Prestito i -> -i
    | Restituzione i -> i

let cassa (s : Stato) : int =
    s.movimenti
    |> List.map (contabile s)
    |> List.sum

let private altroContabile (s : Stato) ((_, op) : Movimento) : int =
    match op with
    | AltraSpesa(_, i) -> -i
    | AltroVersamento(_, i) -> i
    | _ -> 0

let private pagamentoScale (s : Stato) ((_, op) : Movimento) : int =
    match op with
    | PagamentoScale -> (snd s.attuale).costoScale
    | _ -> 0

let dataToDateTime (Data(y, m, d)) = new DateTime(y, m, d)
let dateTimeToData (d : DateTime) = Data(d.Year, d.Month, d.Day)

let tesoretto (s : Stato) : int =
    let altro =
        s.movimenti
        |> List.map (altroContabile s)
        |> List.sum

    let pagamenti =
        s.movimenti
        |> List.map (pagamentoScale s)
        |> List.sum

    let mesi = (new DateDiff(dataToDateTime s.tempoZero, DateTime.Today)).Months
    let num_condomini = List.length s.condomini
    mesi * num_condomini * (snd s.attuale).quotaMensile + altro - pagamenti

let private versamentoQuote (s : Stato) (con : Condomino) ((_, op) : Movimento) : int =
    match op with
    | VersamentoQuote(c, i) when c = con -> i
    | _ -> 0

let quote (s : Stato) (c : Condomino) : int =
    let versamentiQuote =
        s.movimenti
        |> List.map (versamentoQuote s c)
        |> List.sum

    let quoteVersate = versamentiQuote / (snd s.attuale).quotaMensile
    let mesi = (new DateDiff(dataToDateTime s.tempoZero, DateTime.Today)).Months
    quoteVersate - mesi

let prestitiContabile (s : Stato) ((_, op) : Movimento) : int =
    match op with
    | Prestito i -> -i
    | Restituzione i -> i
    | _ -> 0

let prestito (s : Stato) : int =
    s.movimenti
    |> List.map (prestitiContabile s)
    |> List.sum
    |> (~-)

(* Printable representations *)

let DataToString(d) = (dataToDateTime d).ToString("dd-MM-yy")

let OperazioneToString (s : Stato) op =
    match op with
    | VersamentoQuote(c, i) ->
        (sprintf "% 4i EUR | " i) + "Versamento quote (" + c.ToString() + ")"
    | PagamentoScale ->
        (sprintf "% 4i EUR | " -(snd s.attuale).costoScale) + "Pagamento scale"
    | AltraSpesa(str, i) ->
        (sprintf "% 4i EUR | " -i) + "Altra spesa (" + str + ")"
    | AltroVersamento(str, i) ->
        (sprintf "% 4i EUR | " i) + "Altro versamento (" + str + ")"
    | Prestito i -> (sprintf "% 4i EUR | " -i) + "Prestito"
    | Restituzione i -> (sprintf "% 4i EUR | " i) + "Restituzione"

let MovimentoToString s ((d, op) : Movimento) =
    DataToString(d) + " | " + (OperazioneToString s op)

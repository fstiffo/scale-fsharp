module Scale

open System
open System.IO
open System.Runtime.Serialization.Formatters.Soap
open Itenso.TimePeriod

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

let createStato fileName : Stato =
    if (File.Exists(fileName)) then
        let sfStato = new SoapFormatter()
        let fsStato = new FileStream(fileName, FileMode.Open)
        sfStato.Deserialize(fsStato) :?> Stato
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
        let fsStato = new FileStream(fileName, FileMode.Create)
        sfStato.Serialize(fsStato, s)
        s

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

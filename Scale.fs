
module Scale

open System
open System.IO
open System.Runtime.Serialization.Formatters.Soap
open Itenso.TimePeriod


[<Serializable>]
type Condomino = Michela | Gerardo | Elena | Giulia

[<Serializable>]
type Operazione = 
    | VersamentoQuote of Condomino * int
    | PagamentoScale
    | AltraSpesa of string * int
    | AltroVersamento of string * int
    | Prestito of int
    | Restituzione of int

[<Serializable>]
type Data = Data of int * int  * int

[<Serializable>]
type Movimento = (Data * Operazione)

[<Serializable>]
type Param = { costo_scale: int; num_pulizie_mese: int; quota_mensile: int }

[<Serializable>]
type Attuale = (Data * Param)

[<Serializable>]
type Stato = {
    tempo_zero: Data;
    attuale: Attuale;
    condomini: Condomino list;
    movimenti: Movimento list    
}

let createStato fileName : Stato =
    if (File.Exists(fileName)) then
        let sfStato = new SoapFormatter()
        let fsStato = new FileStream(fileName, FileMode.Open)
        sfStato.Deserialize(fsStato) :?> Stato
    else
        let t_0 = Data (2019,7,1)
        let s : Stato = {
               tempo_zero = t_0;
               attuale = (t_0, { costo_scale = 20; num_pulizie_mese=2; quota_mensile=12 });
               condomini = [Michela; Gerardo; Elena; Giulia];
               movimenti = [
                 (t_0, (AltroVersamento ("Appianamento", 333)));
                 (t_0, VersamentoQuote (Michela, 74));
                 (t_0, VersamentoQuote (Gerardo, 78));
                 (t_0, VersamentoQuote (Elena, 48));
                 (Data (2019, 7, 22), Prestito 500);
                 (Data (2019, 7, 11), PagamentoScale)
               ]
             }
        let sfStato = new SoapFormatter()        
        let fsStato = new FileStream(fileName, FileMode.Create)
        sfStato.Serialize(fsStato, s)
        s

let contabile (s: Stato) (op: Operazione) : int = 
    match op with
     | VersamentoQuote (_, i) -> i
     | PagamentoScale -> -(snd s.attuale).costo_scale
     | AltraSpesa (_, i) -> - i
     | AltroVersamento (_, i) -> i
     | Prestito i -> - i
     | Restituzione i -> i

let cassa (s: Stato) : int =
    let ms = s.movimenti in
    ms |> List.map (fun ((_, op) : Movimento) -> contabile s op) |> List.sum

let altroContabile (s: Stato) (op: Operazione) : int =
    match op with
    | AltraSpesa (_, i) -> -i
    | AltroVersamento (_, i) -> i
    | _ -> 0

let dataToDateTime(Data (y,m,d)) =
    new DateTime(y,m,d)

let tesoretto (s: Stato)  : int =
    let ms = s.movimenti
    let altro = ms |> List.map (fun ((_, op) : Movimento) -> altroContabile s op) |> List.sum
    let pagamenti = ms |> List.map (fun ((_, op) : Movimento) -> match op with | PagamentoScale -> (snd s.attuale).costo_scale | _ -> 0) |> List.sum
    let mesi = (new DateDiff(dataToDateTime s.tempo_zero, DateTime.Today)).Months
    let num_condomini = List.length s.condomini
    mesi * num_condomini * (snd s.attuale).quota_mensile + altro - pagamenti
    
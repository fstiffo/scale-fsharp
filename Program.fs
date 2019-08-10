// Learn more about F# at http://fsharp.org

open System
open System.IO
open Terminal.Gui
open NStack
open System.Runtime.Serialization.Formatters.Soap

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
type Scale = {
    tempo_zero: Data;
    attuale: Attuale;
    condomini: Condomino list;
    movimenti: Movimento list    
}




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
    let sfOperazione : SoapFormatter = new SoapFormatter()

    let fsOperazione = new FileStream(@"D:\Users\Francesco\Documents\F#\scale\operazione.scl", FileMode.Create)

    let t_0 = Data (2019,7,1)
    let s : Scale = {
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
    sfOperazione.Serialize(fsOperazione, s)

    Application.Init ()
    let top = Application.Top
    let win = Window (ustr "Hello", X=Pos.op_Implicit(0), Y=Pos.op_Implicit(1), Width=Dim.Fill(), Height=Dim.Fill())
    top.Add (buildMenu())
    top.Add (win)
    Application.Run ()
    0 // return an integer exit code
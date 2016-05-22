namespace Freya.Machines.Http.Cors

#nowarn "46"

(* Preflight *)

[<AutoOpen>]
module Extension =

    (* Extension *)

    let cors =
        set [ Preflight.component ]

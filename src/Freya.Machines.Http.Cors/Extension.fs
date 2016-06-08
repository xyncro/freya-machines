namespace Freya.Machines.Http.Cors

open Freya.Machines
open Freya.Machines.Http.Cors.Machine.Components

#nowarn "46"

(* Preflight *)

[<AutoOpen>]
module Extension =

    (* Extension *)

    let cors =
        set [ Cors.component ]

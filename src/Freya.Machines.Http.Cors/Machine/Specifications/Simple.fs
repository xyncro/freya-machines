namespace Freya.Machines.Http.Cors

open Freya.Core.Operators
open Freya.Machines
open Freya.Optics.Http.Cors

(* Simple *)

[<RequireQualifiedAccess>]
module Simple =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "simple" ]

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        let rec internal hasOrigin p s =
            Decision.create (key p, "has-origin")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.origin_))
                (s, s)

    (* Specification *)

    let internal specification =
        Decisions.hasOrigin
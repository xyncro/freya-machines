namespace Freya.Machines.Http.Cors.Machine.Specifications

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

        let rec internal hasOrigin k s =
            Decision.create (key k, "has-origin")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.origin_))
                (s, originMatches k s)

        and internal originMatches k s =
            Decision.create (key k, "origin-matches")
                (function | _ -> Static false)
                (s, s)

    (* Specification *)

    let internal specification =
        Decisions.hasOrigin
namespace Freya.Machines.Http.Cors.Machine.Specifications

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors
open Freya.Optics.Http.Cors
open Hephaestus

(* Simple *)

[<RequireQualifiedAccess>]
module Simple =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "simple" ]

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        let rec enabled k s =
            Common.Decisions.enabled (key k)
                (s, hasOrigin k s)

        and hasOrigin k s =
            Common.Decisions.hasOrigin (key k)
                (s, originAllowed k s)

        and originAllowed k s =
            Common.Decisions.originAllowed (key k)
                (s, simple k s)

        // TODO: Set origin based on origins + credential support, plus matched origin
        // TODO: Set credential support
        // TODO: Set exposed headers
        // TODO: Consider modularisation of this

        and simple k s =
            Decision.create (key k, "simple")
                (function | _ -> Dynamic (
                                    !. Request.Headers.origin_
                                >>= function | Some (Origin x) -> Operations.simple x
                                             | _ -> Freya.empty
                                >>= function | _ -> Freya.init true))
                (Specification.Terminal.empty, s)

    (* Specification *)

    let specification =
        Decisions.enabled
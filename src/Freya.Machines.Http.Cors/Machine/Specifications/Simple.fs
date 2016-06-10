namespace Freya.Machines.Http.Cors.Machine.Specifications

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Optics.Http.Cors
open Hephaestus

(* Simple *)

[<RequireQualifiedAccess>]
module Simple =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "simple" ]

    (* Aliases

       Shorthand/abbreviations for common functions, used locally to make the
       code more concise where these verbose formulations make the logic harder
       to read. *)

    (* Monadic *)

    let private lift =
        Freya.Value.lift

    (* Optics *)

    let private allowedOrigins_ =
        Properties.Resource.allowedOrigins_

    (* Types *)

    type private O =
        OriginListOrNull

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        let rec internal hasOrigin k s =
            Decision.create (key k, "has-origin")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.origin_))
                (s, originAllowed k s)

        and internal originAllowed k s =
            Decision.create (key k, "origin-allowed")
                (function | TryGet allowedOrigins_ (Static Any) | Get allowedOrigins_ None -> Static true
                          | TryGet allowedOrigins_ (Static (Origins (O.Origins []))) -> Static false
                          | TryGet allowedOrigins_ x -> Dynamic (allow <!> lift x <*> !. Request.Headers.origin_)
                          | _ -> Static false)
                (s, simple k s)

        and private allow range origin =
            match range, origin with
            | Origins (O.Origins xs), Some (Origin (O.Origins [ x ])) when List.contains x xs -> true
            | Any, _ -> true
            | _ -> false

        and internal simple k s =
            Decision.create (key k, "simple")
                (function | _ -> Dynamic (
                                    !. Request.Headers.origin_
                                >>= function | Some (Origin x) -> Operations.simple x
                                             | _ -> Freya.empty
                                >>= function | _ -> Freya.init true))
                (Specification.Terminal.empty, s)

    (* Specification *)

    let internal specification =
        Decisions.hasOrigin
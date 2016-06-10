namespace Freya.Machines.Http.Cors.Machine.Specifications

open Arachne.Http.Cors
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors.Machine.Configuration
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

        type private O =
            OriginListOrNull

        let private lift =
            Freya.Value.lift

        let private allowedOrigins_ =
            Properties.Resource.allowedOrigins_

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
                (s, s)

        and private allow range origin =
            match range, origin with
            | Any, _ -> true
            | Origins (O.Origins xs), Some (Origin (O.Origins [ x ])) when List.contains x xs -> true
            | _ -> false

    (* Specification *)

    let internal specification =
        Decisions.hasOrigin
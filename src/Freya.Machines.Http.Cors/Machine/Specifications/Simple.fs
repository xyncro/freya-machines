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

    let private origins_ =
        Properties.Resource.origins_

    (* Types *)

    type private O =
        OriginListOrNull

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        let rec enabled k s =
            Decision.create (key k, "enabled")
                (function | TryGetOrElse Properties.Extension.enabled_ (Static true) x -> x)
                (s, hasOrigin k s)

        and hasOrigin k s =
            Decision.create (key k, "has-origin")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.origin_))
                (s, originAllowed k s)

        and originAllowed k s =
            Decision.create (key k, "origin-allowed")
                (function | Get origins_ None -> Static true
                          | TryGet origins_ (Static []) -> Static false
                          | TryGet origins_ x -> Dynamic (allow <!> lift x <*> !. Request.Headers.origin_)
                          | _ -> Static false)
                (s, simple k s)

        and private allow origins =
            function | Some (Origin (O.Origins [ x ])) when List.contains x origins -> true
                     | _ -> false

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
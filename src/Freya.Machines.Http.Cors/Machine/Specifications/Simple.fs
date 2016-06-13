namespace Freya.Machines.Http.Cors.Machine.Specifications

open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors.Machine.Configuration
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

        (* Enabled *)

        let rec enabled k s =
            Common.Decisions.enabled (key k)
                (s, hasOrigin k s)

        (* Origin *)

        and hasOrigin k s =
            Common.Decisions.hasOrigin (key k)
                (s, originAllowed k s)

        and originAllowed k s =
            Common.Decisions.originAllowed (key k)
                (s, simple k s)

        (* Simple *)

        and simple k s =
            Decision.create (key k, "simple")
                (function |   TryGetOrElse Properties.Resource.supportsCredentials_ (Static true) supportsCredentials
                            & Get Properties.Resource.origins_ origins
                            & Get Properties.Resource.exposedHeaders_ exposedHeaders ->
                                Dynamic (
                                    Common.allowOriginAndSupportsCredentials supportsCredentials origins
                                 *> Common.exposeHeaders exposedHeaders
                                 *> Freya.init true))
                (Specification.Terminal.empty, s)

    (* Specification *)

    let specification =
        Decisions.enabled
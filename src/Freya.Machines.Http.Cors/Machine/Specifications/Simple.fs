namespace Freya.Machines.Http.Cors.Machine.Specifications

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

        and simple k s =
            Decision.create (key k, "simple")
                (function |   TryGetOrElse Properties.Resource.supportsCredentials_ (Static true) supportsCredentials
                            & Get Properties.Resource.origins_ origins
                            & Get Properties.Resource.exposedHeaders_ exposedHeaders ->
                                Dynamic (
                                        Freya.Optic.get Request.Headers.origin_
                                    >>= fun origin ->
                                        Freya.Value.lift supportsCredentials
                                    >>= fun supportsCredentials ->
                                        Freya.Value.liftOption origins
                                    >>= fun origins ->
                                        Operations.allowOriginAndSupportsCredentials origin (supportsCredentials, origins)
                                    >>= fun _ ->
                                        Freya.Value.liftOption exposedHeaders
                                    >>= fun exposedHeaders ->
                                        Operations.exposeHeaders exposedHeaders
                                    >>= fun _ ->
                                        Freya.init true))
                (Specification.Terminal.empty, s)

    (* Specification *)

    let specification =
        Decisions.enabled
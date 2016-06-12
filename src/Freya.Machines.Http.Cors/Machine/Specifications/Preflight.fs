namespace Freya.Machines.Http.Cors.Machine.Specifications

#nowarn "46"

open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Machine.Specifications
open Freya.Optics.Http
open Freya.Optics.Http.Cors
open Hephaestus

(* Preflight *)

[<RequireQualifiedAccess>]
module Preflight =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "preflight" ]

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
                (s, hasRequestMethod k s)

        and hasRequestMethod k s =
            Decision.create (key k, "has-request-method")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.accessControlRequestMethod_))
                (s, requestMethodMatches k s)

        and requestMethodMatches k s =
            Decision.create (key k, "request-method-matches")
                (function |   Get Properties.Resource.methods_ corsMethods
                            & Get Properties.Request.methods_ methods ->
                                Dynamic (
                                        Freya.Optic.get Request.method_
                                    >>= fun method ->
                                        Freya.Value.liftOption corsMethods
                                    >>= fun corsMethods ->
                                        Freya.Value.liftOption methods
                                    >>= fun methods ->
                                        [ corsMethods; methods; Some Defaults.methods ]
                                        |> List.pick id
                                        |> Set.contains method
                                        |> Freya.init))
                (s, hasRequestHeaders k s)

        and hasRequestHeaders k s =
            Decision.create (key k, "has-request-headers")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.accessControlRequestHeaders_))
                (preflight k s, requestHeadersMatch k s)

        // TODO: logic

        and requestHeadersMatch k s =
            Decision.create (key k, "request-headers-match")
                (function | _ -> Static true)
                (s, preflight k s)

        // TODO: logic

        and preflight k s =
            Decision.create (key k, "preflight")
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
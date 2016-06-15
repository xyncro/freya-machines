namespace Freya.Machines.Http.Cors.Machine.Specifications

#nowarn "46"

open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Machine.Specifications
open Freya.Optics.Http
open Freya.Optics.Http.Cors
open Freya.Types.Http.Cors
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
                (s, hasRequestMethod k s)

        (* Request Method *)

        and hasRequestMethod k s =
            Decision.create (key k, "has-request-method")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.accessControlRequestMethod_))
                (s, requestMethodMatches k s)

        and requestMethodMatches k s =
            Decision.create (key k, "request-method-matches")
                (function |   Get Properties.Resource.methods_ corsMethods
                            & Get Properties.Request.methods_ methods -> Dynamic (methodMatches corsMethods methods))
                (s, hasRequestHeaders k s)

        and private methodMatches corsMethods methods =
                Freya.Optic.get Request.method_
            >>= fun method ->
                Freya.Value.liftOption corsMethods
            >>= fun corsMethods ->
                Freya.Value.liftOption methods
            >>= fun methods ->
                [ corsMethods; methods; Some Defaults.methods ]
                |> List.pick id
                |> Set.contains method
                |> Freya.init

        (* Request Headers *)

        and hasRequestHeaders k s =
            Decision.create (key k, "has-request-headers")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.accessControlRequestHeaders_))
                (preflight k s, requestHeadersMatch k s)

        and requestHeadersMatch k s =
            Decision.create (key k, "request-headers-match")
                (function | Get Properties.Resource.headers_ headers -> Dynamic (headersMatch headers))
                (s, preflight k s)

        and private headersMatch headers =
                Freya.Optic.get Request.Headers.accessControlRequestHeaders_
            >>= fun requestHeaders ->
                Freya.Value.liftOption headers
            >>= fun headers ->
                (requestHeaders, headers)
                |> function | Some (AccessControlRequestHeaders rhs), Some hs ->
                                    Set.exists (String.equalsIgnoreCase >> flip List.exists rhs) hs
                            | _ -> true
                |> Freya.init

        (* Preflight *)

        and preflight k s =
            Decision.create (key k, "preflight")
                (function |   TryGetOrElse Properties.Resource.supportsCredentials_ (Static true) supportsCredentials
                            & Get Properties.Resource.origins_ origins
                            & Get Properties.Resource.maxAge_ maxAge ->
                                Dynamic (
                                    Common.allowOriginAndSupportsCredentials supportsCredentials origins
                                 *> Common.maxAge maxAge
                                 *> Common.allowMethods
                                 *> Common.allowHeaders
                                 *> Freya.init true))
                (Specification.Terminal.empty, s)

    (* Specification *)

    let specification =
        Decisions.enabled
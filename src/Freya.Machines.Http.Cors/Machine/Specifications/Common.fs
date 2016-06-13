namespace Freya.Machines.Http.Cors.Machine.Specifications

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Cors
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Optics.Http.Cors

(* Common *)

[<RequireQualifiedAccess>]
module Common =

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

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        (* Enabled *)

        let enabled k =
            Decision.create (k, "enabled")
                (function | TryGetOrElse Extension.enabled_ (Static true) x -> x)

        (* Origin *)

        let hasOrigin k =
            Decision.create (k, "has-origin")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.origin_))

        let rec originAllowed k =
            Decision.create (k, "origin-allowed")
                (function | Get origins_ None -> Static true
                          | TryGet origins_ (Static x) when Set.isEmpty x -> Static false
                          | TryGet origins_ x -> Dynamic (allow <!> lift x <*> !. Request.Headers.origin_)
                          | _ -> Static false)

        and private allow origins =
            function | Some (Origin (OriginListOrNull.Origins [ x ])) when Set.contains x origins -> true
                     | _ -> false

    (* Support *)

    let internal allowHeaders =
            Freya.Optic.get Request.Headers.accessControlRequestHeaders_
        >>= Operations.allowHeaders

    let internal allowMethods =
            Freya.Optic.get Request.Headers.accessControlRequestMethod_
        >>= Operations.allowMethods

    let internal allowOriginAndSupportsCredentials supportsCredentials origins =
            Freya.Optic.get Request.Headers.origin_
        >>= fun origin ->
            Freya.Value.lift supportsCredentials
        >>= fun supportsCredentials ->
            Freya.Value.liftOption origins
        >>= fun origins ->
            Operations.allowOriginAndSupportsCredentials origin supportsCredentials origins

    let internal exposeHeaders exposedHeaders =
            Freya.Value.liftOption exposedHeaders
        >>= Operations.exposeHeaders

    let internal maxAge maxAge =
            Freya.Value.liftOption maxAge
        >>= Operations.maxAge
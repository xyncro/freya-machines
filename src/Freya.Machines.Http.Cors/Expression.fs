namespace Freya.Machines.Http.Cors

#nowarn "46"

open Freya.Machines.Http
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Machines.Http.Cors.Machine.Components

(* Use *)

[<RequireQualifiedAccess>]
module Use =

    let cors =
        set [ Cors.component ]

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Extension *)

    type HttpMachineBuilder with

        /// Includes the CORS components in the Freya HTTP machine.
        [<CustomOperation ("cors", MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Cors (m) =
            x.Using (m, Use.cors)

    type HttpMachineBuilder with

        /// Determines whether the CORS components are enabled for the current
        /// request.
        [<CustomOperation ("corsEnabled", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsEnabled (m, a) =
            HttpMachine.set (m, Extension.enabled_, Decision.infer a)

    (* Properties *)

    type HttpMachineBuilder with

        /// Specifies the (list of) valid CORS origins.
        [<CustomOperation ("corsOrigins", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsOrigins (m, a) =
            HttpMachine.set (m, Properties.Resource.origins_, Origins.infer a)

        /// Specifies the (list of) valid CORS methods, if different from the
        /// methods specified with `methods`.
        [<CustomOperation ("corsMethods", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsMethods (m, a) =
            HttpMachine.set (m, Properties.Resource.methods_, Methods.infer a)

        /// Specifies the (list of) valid CORS headers.
        [<CustomOperation ("corsHeaders", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsHeaders (m, a) =
            HttpMachine.set (m, Properties.Resource.headers_, Headers.infer a)

        /// Specifies the (list of) headers that a browser is allowed to access.
        [<CustomOperation ("corsExposedHeaders", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsExposedHeaders (m, a) =
            HttpMachine.set (m, Properties.Resource.exposedHeaders_, Headers.infer a)

        /// Specifies whether the CORS request is allowed to use the user's
        /// credentials of the server
        [<CustomOperation ("corsSupportsCredentials", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsSupportsCredentials (m, a) =
            HttpMachine.set (m, Properties.Resource.supportsCredentials_, Decision.infer a)

        /// Specifies the maximum timespan in seconds that a client can cache
        /// CORS header data before re-validating.
        [<CustomOperation ("corsMaxAge", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsMaxAge (m, a) =
            HttpMachine.set (m, Properties.Resource.maxAge_, TimeSpan.infer a)

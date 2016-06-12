namespace Freya.Machines.Http.Cors

#nowarn "46"

open Freya.Machines.Http
open Freya.Machines.Http.Cors.Machine.Configuration
open Freya.Machines.Http.Cors.Machine.Components

(* Use *)

[<AutoOpen>]
module Use =

    let cors =
        set [ Cors.component ]

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Extension *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsEnabled", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsEnabled (m, a) =
            HttpMachine.set (m, Extension.enabled_, Decision.infer a)

    (* Properties *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsOrigins", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsOrigins (m, a) =
            HttpMachine.set (m, Properties.Resource.origins_, Origins.infer a)

        [<CustomOperation ("corsMethods", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsMethods (m, a) =
            HttpMachine.set (m, Properties.Resource.methods_, Methods.infer a)

        [<CustomOperation ("corsHeaders", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsHeaders (m, a) =
            HttpMachine.set (m, Properties.Resource.headers_, Headers.infer a)

        [<CustomOperation ("corsExposedHeaders", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsExposedHeaders (m, a) =
            HttpMachine.set (m, Properties.Resource.exposedHeaders_, Headers.infer a)

        [<CustomOperation ("corsSupportsCredentials", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsSupportsCredentials (m, a) =
            HttpMachine.set (m, Properties.Resource.supportsCredentials_, Decision.infer a)

        [<CustomOperation ("corsMaxAge", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsMaxAge (m, a) =
            HttpMachine.set (m, Properties.Resource.maxAge_, Integer.infer a)
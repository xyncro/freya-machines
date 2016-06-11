namespace Freya.Machines.Http.Cors

open Freya.Machines.Http
open Freya.Machines.Http.Cors.Machine.Configuration

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Extension *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsEnabled", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsEnabled (m, a) =
            HttpMachine.set (m, Properties.Extension.enabled_, Decision.infer a)

    (* Properties *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsOrigins", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsOrigins (m, a) =
            HttpMachine.set (m, Properties.Resource.origins_, Origins.infer a)
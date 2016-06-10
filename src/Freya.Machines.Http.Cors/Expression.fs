namespace Freya.Machines.Http.Cors

open Freya.Machines.Http
open Freya.Machines.Http.Cors.Machine.Configuration

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Properties *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsAllowedOrigins", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsAllowedOrigins (m, a) =
            HttpMachine.set (m, Properties.Resource.allowedOrigins_, AccessControlAllowOriginRange.infer a)
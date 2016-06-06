namespace Freya.Machines.Http.Cors

open Freya.Machines.Http

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Origins *)

    type HttpMachineBuilder with

        [<CustomOperation ("corsAllowedOrigins", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CorsAllowedOrigins (m, a) =
            HttpMachine.set (m, Properties.Resource.allowedOrigins_, a)
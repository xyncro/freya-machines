namespace Freya.Machines.Http.Cors

open Arachne.Http.Cors
open Freya.Core
open Freya.Machines

(* Inference *)

[<AutoOpen>]
module Inference =

    [<RequireQualifiedAccess>]
    module AccessControlAllowOriginRange =

        (* Inference *)

        [<RequireQualifiedAccess>]
        module Inference =

            type Defaults =
                | Defaults

                static member AccessControlAllowOriginRange (x: Freya<AccessControlAllowOriginRange>) =
                    Dynamic x

                static member AccessControlAllowOriginRange (x: AccessControlAllowOriginRange) =
                    Static x

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member AccessControlAllowOriginRange: ^a -> Value<AccessControlAllowOriginRange>) a)

            let inline infer (x: 'a) =
                defaults (x, Defaults)

        let inline infer v =
            Inference.infer v
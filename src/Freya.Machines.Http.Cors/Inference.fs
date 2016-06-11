namespace Freya.Machines.Http.Cors

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Machines

(* Inference *)

[<AutoOpen>]
module Inference =

    [<RequireQualifiedAccess>]
    module Origins =

        (* Inference *)

        [<RequireQualifiedAccess>]
        module Inference =

            type Defaults =
                | Defaults

                static member Origins (x: Freya<SerializedOrigin list>) =
                    Dynamic x

                static member Origins (x: Freya<SerializedOrigin>) =
                    Dynamic (List.singleton <!> x)

                static member Origins (x: SerializedOrigin list) =
                    Static x

                static member Origins (x: SerializedOrigin) =
                    Static (List.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member Origins: ^a -> Value<SerializedOrigin list>) a)

            let inline infer (x: 'a) =
                defaults (x, Defaults)

        let inline infer v =
            Inference.infer v
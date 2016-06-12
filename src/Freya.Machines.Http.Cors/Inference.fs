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
                    Dynamic (Set.ofList <!> x)

                static member Origins (x: Freya<SerializedOrigin>) =
                    Dynamic (Set.singleton <!> x)

                static member Origins (x: SerializedOrigin list) =
                    Static (Set.ofList x)

                static member Origins (x: SerializedOrigin) =
                    Static (Set.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member Origins: ^a -> Value<Set<SerializedOrigin>>) a)

            let inline infer (x: 'a) =
                defaults (x, Defaults)

        let inline infer v =
            Inference.infer v
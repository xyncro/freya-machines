namespace Freya.Machines.Http

#nowarn "46"

open Hephaestus

(* Http *)

[<RequireQualifiedAccess>]
module internal Http =

    let private components =
        set [
            Core.component
            Delete.component
            GetOrHead.component
            Options.component
            Post.component
            Put.component ]

    let model extensions =
        Model.create (Set.union components extensions)
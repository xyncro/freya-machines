namespace Freya.Machines.Http

#nowarn "46"

open Hephaestus

(* Http *)

[<RequireQualifiedAccess>]
module internal Http =

    let model =
        Model.create (
            set [
                Core.component
                Delete.component
                GetOrHead.component
                Options.component
                Post.component
                Put.component ])
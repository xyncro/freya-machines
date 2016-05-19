namespace Freya.Machines.Http.Machine.Models

#nowarn "46"

open Freya.Machines.Http.Machine.Components
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
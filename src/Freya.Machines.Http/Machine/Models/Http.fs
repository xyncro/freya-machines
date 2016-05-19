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
                Core.component ])
//                GetOrHead.export
//                Options.export
//                Post.export
//                Put.export
//                Delete.export ])
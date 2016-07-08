namespace Freya.Machines.Http.Patch

#nowarn "46"

open Freya.Machines.Http
open Freya.Machines.Http.Machine.Specifications
open Freya.Machines.Http.Patch.Machine.Components
open Freya.Machines.Http.Patch.Machine.Configuration
open Freya.Types.Http.Patch

(* Use *)

[<RequireQualifiedAccess>]
module Use =

    let patch =
        set [ Patch.component ]

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Extension *)

    type HttpMachineBuilder with

        [<CustomOperation ("patch", MaintainsVariableSpaceUsingBind = true)>]
        member inline x.Patch (m) =
            x.Using (m, Use.patch)

    type HttpMachineBuilder with

        [<CustomOperation ("patchEnabled", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.PatchEnabled (m, a) =
            HttpMachine.set (m, Extension.enabled_, Decision.infer a)

    (* Properties *)

    type HttpMachineBuilder with

        (* Request *)

        [<CustomOperation ("patchAcceptableMediaTypes", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.PatchAcceptableMediaTypes (m, a) =
            HttpMachine.set (m, Properties.Request.mediaTypes_, AcceptableMedia.infer a)

    (* Operations *)

    type HttpMachineBuilder with

        (* Operations *)

        [<CustomOperation ("patchDoPatch", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.PatchDoPatch (m, a) =
            HttpMachine.set (m, (Operation.Decisions.operationMethod_ PATCH), Operation.infer a)
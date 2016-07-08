namespace Freya.Machines.Http.Patch.Machine.Specifications

open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Patch
open Freya.Machines.Http.Patch.Machine.Configuration

(* Common *)

[<RequireQualifiedAccess>]
module Common =

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        (* Enabled *)

        let enabled k =
            Decision.create (k, "enabled")
                (function | TryGetOrElse Extension.enabled_ (Static true) x -> x)

    (* Support *)

    let internal acceptPatch mediaTypes =
            Freya.Value.lift mediaTypes
        >>= Operations.acceptPatch
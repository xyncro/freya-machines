namespace Freya.Machines.Http.Patch.Machine.Specifications

open Freya.Machines

(* Main *)

[<RequireQualifiedAccess>]
module Main =

    (* Key *)

    let private key =
        Key.root

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        (* Enabled *)

        let enabled k =
            Common.Decisions.enabled (key k)

    let specification =
        Decisions.enabled
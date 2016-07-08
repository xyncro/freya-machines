namespace Freya.Machines.Http.Patch.Machine.Specifications

open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Patch.Machine.Configuration
open Freya.Types.Http.Patch
open Hephaestus

(* Headers *)

[<RequireQualifiedAccess>]
module Headers =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "headers" ]

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        (* Enabled *)

        let rec enabled k s =
            Common.Decisions.enabled (key k)
                (s, acceptPatch k s)

        (* Allow *)

        and allowPatch k s =
            Decision.create (key k, "allow-patch")
                (function | TryGet Properties.Request.methods_ (Static x) -> Static (contains x)
                          | TryGet Properties.Request.methods_ (Dynamic x) -> Dynamic (contains <!> x)
                          | _ -> Static false)
                (s, acceptPatch k s)

        and private contains =
            Set.contains PATCH

        (* Accept-Patch *)

        and acceptPatch k s =
            Decision.create (key k, "accept-patch")
                (function | TryGet Properties.Request.mediaTypes_ mediaTypes ->
                                Dynamic (
                                    Common.acceptPatch mediaTypes
                                 *> Freya.init true)
                          | _ -> Static true)
                (Specification.Terminal.empty, s)

    let specification =
        Decisions.enabled
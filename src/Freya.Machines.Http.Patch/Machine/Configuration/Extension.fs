namespace Freya.Machines.Http.Patch.Machine.Configuration

open Aether.Operators
open Freya.Machines

(* Extension *)

[<RequireQualifiedAccess>]
module Extension =

    (* Types *)

    type private Extension =
        { Enabled: Value<bool> option }

        static member enabled_ =
            (fun x -> x.Enabled), (fun e x -> { x with Enabled = e })

        static member empty =
            { Enabled = None }

    (* Optics *)

    let private extension_ =
        Configuration.element_ Extension.empty [ "http-patch"; "configuration"; "extension" ]

    let enabled_ =
            extension_
        >-> Extension.enabled_

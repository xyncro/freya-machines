[<AutoOpen>]
module Freya.Machines.Http.Cors.Tests.Prelude

open Arachne.Http.Cors
open Arachne.Uri

(* Fixtures *)

[<RequireQualifiedAccess>]
module Xyncro =

    let com =
        SerializedOrigin (
            Scheme "http",
            Name (RegName "xyncro.com"),
            None)
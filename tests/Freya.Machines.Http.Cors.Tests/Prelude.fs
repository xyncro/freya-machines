[<AutoOpen>]
module Freya.Machines.Http.Cors.Tests.Prelude

open Freya.Types.Http.Cors
open Freya.Types.Uri

(* Fixtures *)

[<RequireQualifiedAccess>]
module Xyncro =

    let com =
        SerializedOrigin (
            Scheme "http",
            Name (RegName "xyncro.com"),
            None)
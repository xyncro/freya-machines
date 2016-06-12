[<AutoOpen>]
module Freya.Machines.Http.Cors.Tests.Prelude

open Aether
open Arachne.Http.Cors
open Arachne.Uri
open Freya.Core

(* Optics *)

let defaultValue : Lens<State,string option> =
    State.value_ "default"

(* Fixtures *)

let defaultSetup =
    Freya.empty

[<RequireQualifiedAccess>]
module Xyncro =

    let com =
        SerializedOrigin (
            Scheme "http",
            Name (RegName "xyncro.com"),
            None)
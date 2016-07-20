[<AutoOpen>]
module Freya.Machines.Http.Patch.Tests.Prelude

open Aether
open Freya.Core

(* Optics *)

let defaultValue : Lens<State,string option> =
    State.value_ "default"
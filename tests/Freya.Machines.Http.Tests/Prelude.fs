[<AutoOpen>]
module Freya.Machines.Http.Tests.Prelude

open Aether
open Freya.Core
open Freya.Machines.Http

(* Optics *)

let defaultValue : Lens<State,string option> =
    State.value_ "default"

(* Fixtures *)

let defaultSetup =
    Freya.empty

let defaultRepresentation =
    Freya.init Representation.empty

(* Machines *)

let defaultMachine =
    freyaHttpMachine {
        return () }
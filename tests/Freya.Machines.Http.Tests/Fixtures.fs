[<AutoOpen>]
module Freya.Machines.Http.Tests.Fixtures

open Freya.Core
open Freya.Machines.Http

(* Fixtures *)

let defaultSetup =
    freya {
        return () }

let defaultHandler message =
    freya {
        do! Freya.Optic.set (State.value_ "test") (Some message)
        return Representation.empty }

let defaultMachine =
    freyaHttpMachine {
        return () }
module Freya.Machines.Http.Tests

open Aether
open Aether.Operators
open Freya.Core
open Freya.Optics.Http
open Freya.Machines.Http
open Freya.Testing
open Swensen.Unquote
open Xunit

(* Tests

   Behavioural tests of a Freya HTTP machine given different configurations and
   inputs, designed to give test cases aligning to the semantics of the HTTP
   specifications where appropriate. *)

(* Defaults *)

let setup =
    Freya.empty

let defaults =
    freyaMachine {
        return () }

[<Fact>]
let ``machine with only defaults returns 200 for GET request`` () =
    Testing.verify setup defaults [
        fun s -> s ^. Response.statusCode_ =! Some 200
        fun s -> s ^. Response.reasonPhrase_ =! Some "OK" ]
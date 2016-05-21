module Freya.Machines.Http.Tests.Operation

open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Operation

   Verification that the Operation element behaves as expected given suitable
   input, including verification that the operation to be run is executed and
   that the result of the operation influences the response path. *)

(* Operation *)

[<Fact>]
let ``machine processes operation correctly`` () =

    let test_ =
        State.value_<string> "test"

    let setup =
        Request.method_ .= POST

    (* Success *)

    let success =
        freya {
            do! Freya.Optic.set test_ (Some "test") }

    let successMachine =
        freyaMachine {
            doPost success
            methods POST }

    verify setup successMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK"
        test_ => Some "test" ]

    (* Failure *)

    let failure =
        freya {
            do! Freya.Optic.set test_ (Some "test")
            return false }

    let failureMachine =
        freyaMachine {
            doPost failure
            methods POST }

    verify setup failureMachine [
        Response.statusCode_ => Some 500
        Response.reasonPhrase_ => Some "Internal Server Error"
        test_ => Some "test" ]

(* Completed *)

[<Fact>]
let ``machine handles completed correctly`` () =

    let setup =
        Request.method_ .= POST

    (* Completed *)

    let completedMachine =
        freyaMachine {
            methods POST }

    verify setup completedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Uncompleted *)

    let uncompletedMachine =
        freyaMachine {
            completed false
            methods POST }

    verify setup uncompletedMachine [
        Response.statusCode_ => Some 202
        Response.reasonPhrase_ => Some "Accepted" ]
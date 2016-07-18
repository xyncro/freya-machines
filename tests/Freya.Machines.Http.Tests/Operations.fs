module Freya.Machines.Http.Tests.Operation

open Freya.Core
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Operation

   Verification that the Operation element behaves as expected given suitable
   input, including verification that the operation to be run is executed and
   that the result of the operation influences the response path. *)

(* Operation *)

[<Fact>]
let ``machine processes operation correctly`` () =

    let setup =
            (Request.method_ .= POST)
         *> (Request.Headers.contentLength_ .= Some (ContentLength 0))

    (* Success *)

    let successMachine =
        freyaHttpMachine {
            doPost (defaultValue .= Some "Success")
            methods POST }

    verify setup successMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK"
        defaultValue => Some "Success" ]

    (* Failure *)

    let failureMachine =
        freyaHttpMachine {
            doPost ((defaultValue .= Some "Failure") *> (Freya.init false))
            methods POST }

    verify setup failureMachine [
        Response.statusCode_ => Some 500
        Response.reasonPhrase_ => Some "Internal Server Error"
        defaultValue => Some "Failure" ]

(* Completed *)

[<Fact>]
let ``machine handles completed correctly`` () =

    let setup =
            (Request.method_ .= POST)
         *> (Request.Headers.contentLength_ .= Some (ContentLength 0))

    (* Completed *)

    let completedMachine =
        freyaHttpMachine {
            methods POST }

    verify setup completedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Uncompleted *)

    let uncompletedMachine =
        freyaHttpMachine {
            completed false
            methods POST }

    verify setup uncompletedMachine [
        Response.statusCode_ => Some 202
        Response.reasonPhrase_ => Some "Accepted" ]
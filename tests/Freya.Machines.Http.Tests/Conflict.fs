module Freya.Machines.Http.Tests.Conflict

open Arachne.Http
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Conflict

   Verification that the Conflict element behaves as expected given suitable
   input. *)

(* Conflict *)

[<Fact>]
let ``machine handles conflict correctly`` () =

    let nonConflictSetup =
            (Request.method_ .= POST)

    let conflictSetup =
            (Request.path_ .= "/conflict")
         *> (Request.method_ .= POST)

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            conflict true
            methods POST }

    verify nonConflictSetup staticMachine [
        Response.statusCode_ => Some 409
        Response.reasonPhrase_ => Some "Conflict" ]

    (* Dynamic *)

    let dynamicMachine =
        freyaHttpMachine {
            conflict ((=) "/conflict" <!> !. Request.path_)
            methods POST }

    verify nonConflictSetup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify conflictSetup dynamicMachine [
        Response.statusCode_ => Some 409
        Response.reasonPhrase_ => Some "Conflict" ]
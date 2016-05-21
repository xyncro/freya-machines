module Freya.Machines.Http.Tests.Responses

open Freya.Core
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Responses

   Verification that the various Responses specifications behave as
   expected. *)

(* Common *)

module Common =

    (* No Content *)

    [<Fact>]
    let ``machine handles nocontent correctly`` () =

        (* Static *)

        let staticMachine =
            freyaMachine {
                noContent true }

        verify defaultSetup staticMachine [
            Response.statusCode_ => Some 204
            Response.reasonPhrase_ => Some "No Content" ]

        (* Dynamic *)

        let setup =
            Request.path_ .= "/nocontent"

        let dynamicMachine =
            freyaMachine {
                noContent ((=) "/nocontent" <!> !. Request.path_) }

        verify defaultSetup dynamicMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify setup dynamicMachine [
            Response.statusCode_ => Some 204
            Response.reasonPhrase_ => Some "No Content" ]
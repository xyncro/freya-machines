module Freya.Machines.Http.Tests.Existence

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Existence

   Verification that the Existence specification behaves as expected given
   suitable input. *)

(* Exists *)

[<Fact>]
let ``machine handles exists decision correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            exists true }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/exists"

    let dynamicMachine =
        freyaHttpMachine {
            exists ((=) "/exists" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 404
        Response.reasonPhrase_ => Some "Not Found" ]
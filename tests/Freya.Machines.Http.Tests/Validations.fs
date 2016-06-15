module Freya.Machines.Http.Tests.Validations

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Validations

   Verification that the Validations specifications behaves as expected given
   suitable input. *)

(* Expectation Met *)

[<Fact>]
let ``machine handles expectationMet correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            expectationMet false }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 417
        Response.reasonPhrase_ => Some "Expectation Failed" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/unmet"

    let dynamicMachine =
        freyaHttpMachine {
            expectationMet ((<>) "/unmet" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 417
        Response.reasonPhrase_ => Some "Expectation Failed" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

(* Method Allowed *)

[<Fact>]
let ``machine handles method allowance correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            methods POST }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 405
        Response.reasonPhrase_ => Some "Method Not Allowed"
        Response.Headers.allow_ => Some (Allow [ POST ]) ]

(* URI Too Long *)

[<Fact>]
let ``machine handles uriTooLong correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            uriTooLong true }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 414
        Response.reasonPhrase_ => Some "URI Too Long" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/uritoolong"

    let dynamicMachine =
        freyaHttpMachine {
            uriTooLong ((=) "/uritoolong" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 414
        Response.reasonPhrase_ => Some "URI Too Long" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

(* Bad Request *)

[<Fact>]
let ``machine handles badRequest correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            badRequest true }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 400
        Response.reasonPhrase_ => Some "Bad Request" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/badrequest"

    let dynamicMachine =
        freyaHttpMachine {
            badRequest ((=) "/badrequest" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 400
        Response.reasonPhrase_ => Some "Bad Request" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]
module Freya.Machines.Http.Tests.Assertions

open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Assertions

   Verification that the Assertions specifications behave as expected given
   suitable input. *)

(* Service Available *)

[<Fact>]
let ``machine handles serviceAvailable correctly`` () =

    (* Static *)

    let staticMachine =
        freyaMachine {
            serviceAvailable false
            handleServiceUnavailable (defaultHandler "Service Unavailable") }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 503
        Response.reasonPhrase_ => Some "Service Unavailable"
        State.value_ "test" => Some "Service Unavailable" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/available"

    let dynamicMachine =
        freyaMachine {
            serviceAvailable ((=) "/available" <!> !. Request.path_)
            handleOk (defaultHandler "OK")
            handleServiceUnavailable (defaultHandler "Service Unavailable") }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK"
        State.value_ "test" => Some "OK" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 503
        Response.reasonPhrase_ => Some "Service Unavailable"
        State.value_ "test" => Some "Service Unavailable" ]

(* Http Version Supported *)

[<Fact>]
let ``machine handles httpVersionSupported correctly`` () =

    (* Static *)

    let staticMachine =
        freyaMachine {
            httpVersionSupported false
            handleNotSupported (defaultHandler "HTTP Version Not Supported") }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 505
        Response.reasonPhrase_ => Some "HTTP Version Not Supported"
        State.value_ "test" => Some "HTTP Version Not Supported" ]

    (* Default *)

    let supportedSetup =
        Request.httpVersion_ .= HTTP 1.1

    let unsupportedSetup =
        Request.httpVersion_ .= HTTP 1.0

    verify supportedSetup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify unsupportedSetup defaultMachine [
        Response.statusCode_ => Some 505
        Response.reasonPhrase_ => Some "HTTP Version Not Supported" ]

(* Method Implemented *)

[<Fact>]
let ``machine handles methodImplemented correctly`` () =

    (* Inferred *)

    let allowedSetup =
        Request.method_ .= Method.Custom "FOO"

    let notAllowedSetup =
        Request.method_ .= Method.Custom "BAR"

    // TODO: How should we handle custom methods!

    let machine =
        freyaMachine {
            methods [ Method.Custom "FOO" ]
            handleNotImplemented (defaultHandler "Not Implemented") }

    verify allowedSetup machine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify notAllowedSetup machine [
        Response.statusCode_ => Some 501
        Response.reasonPhrase_ => Some "Not Implemented"
        State.value_ "test" => Some "Not Implemented" ]

    (* Default *)

    let notImplementedSetup =
        Request.method_ .= Method.Custom "FOO"

    verify defaultSetup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify notImplementedSetup defaultMachine [
        Response.statusCode_ => Some 501
        Response.reasonPhrase_ => Some "Not Implemented" ]
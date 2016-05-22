module Freya.Machines.Http.Tests.Assertions

open Arachne.Http
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
        freyaHttpMachine {
            serviceAvailable false
            handleServiceUnavailable ((defaultValue .= Some "Service Unavailable") *> defaultRepresentation) }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 503
        Response.reasonPhrase_ => Some "Service Unavailable"
        defaultValue => Some "Service Unavailable" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/available"

    let dynamicMachine =
        freyaHttpMachine {
            serviceAvailable ((=) "/available" <!> !. Request.path_)
            handleOk ((defaultValue .= Some "OK") *> defaultRepresentation)
            handleServiceUnavailable ((defaultValue .= Some "Service Unavailable") *> defaultRepresentation) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK"
        defaultValue => Some "OK" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 503
        Response.reasonPhrase_ => Some "Service Unavailable"
        defaultValue => Some "Service Unavailable" ]

(* Http Version Supported *)

[<Fact>]
let ``machine handles httpVersionSupported correctly`` () =

    (* Static *)

    let staticMachine =
        freyaHttpMachine {
            httpVersionSupported false
            handleNotSupported ((defaultValue .= Some "HTTP Version Not Supported") *> defaultRepresentation) }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 505
        Response.reasonPhrase_ => Some "HTTP Version Not Supported"
        defaultValue => Some "HTTP Version Not Supported" ]

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

    let machine =
        freyaHttpMachine {
            methods [ Method.Custom "FOO" ]
            handleNotImplemented ((defaultValue .= Some "Not Implemented") *> defaultRepresentation) }

    verify allowedSetup machine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify notAllowedSetup machine [
        Response.statusCode_ => Some 501
        Response.reasonPhrase_ => Some "Not Implemented"
        defaultValue => Some "Not Implemented" ]

    (* Default *)

    let notImplementedSetup =
        Request.method_ .= Method.Custom "FOO"

    verify defaultSetup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify notImplementedSetup defaultMachine [
        Response.statusCode_ => Some 501
        Response.reasonPhrase_ => Some "Not Implemented" ]
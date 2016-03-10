module Freya.Machines.Http.Tests

open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Machines.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Tests

   Behavioural tests of a Freya HTTP machine given different configurations and
   inputs, designed to give test cases aligning to the semantics of the HTTP
   specifications where appropriate. *)

let defaultSetup =
    Freya.empty

let defaultMachine =
    freyaMachine {
        return () }

(* Defaults

   Verification of default behaviour of an unconfigured Freya HTTP machine.
   Only relatively simple behaviour can be expected, but verification of status
   codes, reason phrases, etc. can be verified, along with the correct set of
   responses allowed, etc. *)

module Defaults =

    [<Fact>]
    let ``default machine handles GET request appropriately`` () =
        verify defaultSetup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

    [<Fact>]
    let ``default machine handles HEAD request appropriately`` () =
        let setup =
            Request.method_ .= HEAD

        verify setup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

    [<Fact>]
    let ``default machine handles OPTIONS request appropriately`` () =
        let setup =
            Request.method_ .= OPTIONS

        verify setup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "Options" ]

    [<Fact>]
    let ``default machine handles POST request appropriately`` () =
        let setup =
            Request.method_ .= POST

        let assertions = [
            Response.statusCode_ => Some 405
            Response.reasonPhrase_ => Some "Method Not Allowed" ]

        verify setup defaultMachine assertions

(* Assertion

   Verification that the Assertion block behaves as expected given suitable
   input. *)

module Assertion =

    (* Service Available *)

    [<Fact>]
    let ``machine handles serviceAvailable correctly`` () =

        (* Static *)

        let staticMachine =
            freyaMachine {
                serviceAvailable false }

        verify defaultSetup staticMachine [
            Response.statusCode_ => Some 503
            Response.reasonPhrase_ => Some "Service Unavailable" ]

        (* Dynamic *)

        let setupAvailable =
            Request.path_ .= "/available"

        let dynamicMachine =
            freyaMachine {
                serviceAvailable ((=) "/available" <!> !. Request.path_) }

        verify setupAvailable dynamicMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify defaultSetup dynamicMachine [
            Response.statusCode_ => Some 503
            Response.reasonPhrase_ => Some "Service Unavailable" ]

    (* Http Version Supported *)

    [<Fact>]
    let ``machine handles httpVersionSupported correctly`` () =

        (* Static *)

        let setup =
            Freya.empty

        let staticMachine =
            freyaMachine {
                httpVersionSupported false }

        verify setup staticMachine [
            Response.statusCode_ => Some 505
            Response.reasonPhrase_ => Some "HTTP Version Not Supported" ]

        (* Default *)

        let setupSupported =
            Request.httpVersion_ .= HTTP 1.1

        let setupUnsupported =
            Request.httpVersion_ .= HTTP 1.0

        verify setupSupported defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify setupUnsupported defaultMachine [
            Response.statusCode_ => Some 505
            Response.reasonPhrase_ => Some "HTTP Version Not Supported" ]

    (* Method Implemented *)

    [<Fact>]
    let ``machine handles methodImplemented correctly`` () =

        (* Inferred *)

        let setupAllowed =
            Request.method_ .= Method.Custom "FOO"

        let setupNotAllowed =
            Request.method_ .= Method.Custom "BAR"

        let machine =
            freyaMachine {
                methodsAllowed [ Method.Custom "FOO" ] }

        verify setupAllowed machine [
            Response.statusCode_ => Some 200 ]

        verify setupNotAllowed machine [
            Response.statusCode_ => Some 501
            Response.reasonPhrase_ => Some "Not Implemented" ]

        (* Default *)

        let setupNotImplemented =
            Request.method_ .= Method.Custom "FOO"

        verify defaultSetup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify setupNotImplemented defaultMachine [
            Response.statusCode_ => Some 501
            Response.reasonPhrase_ => Some "Not Implemented" ]

(* Permission

   Verification that the Permission block behaves as expected given suitable
   input. *)

module Permission =

    (* Authorized *)

    [<Fact>]
    let ``machine handles authorized correctly`` () =

        (* Static *)

        let staticMachine =
            freyaMachine {
                authorized false }

        verify defaultSetup staticMachine [
            Response.statusCode_ => Some 401
            Response.reasonPhrase_ => Some "Unauthorized" ]

        (* Dynamic *)

        let setupAuthorized =
            Request.path_ .= "/authorized"

        let dynamicMachine =
            freyaMachine {
                authorized ((=) "/authorized" <!> !. Request.path_) }

        verify setupAuthorized dynamicMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify defaultSetup dynamicMachine [
            Response.statusCode_ => Some 401
            Response.reasonPhrase_ => Some "Unauthorized" ]

    (* Allowed *)

    [<Fact>]
    let ``machine handles allowed correctly`` () =

        (* Static *)

        let staticMachine =
            freyaMachine {
                allowed false }

        verify defaultSetup staticMachine [
            Response.statusCode_ => Some 403
            Response.reasonPhrase_ => Some "Forbidden" ]

        (* Dynamic *)

        let setupAuthorized =
            Request.path_ .= "/allowed"

        let dynamicMachine =
            freyaMachine {
                allowed ((=) "/allowed" <!> !. Request.path_) }

        verify setupAuthorized dynamicMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

        verify defaultSetup dynamicMachine [
            Response.statusCode_ => Some 403
            Response.reasonPhrase_ => Some "Forbidden" ]
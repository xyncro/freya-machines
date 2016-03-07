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

(* Defaults

   Verification of default behaviour of an unconfigured Freya HTTP machine.
   Only relatively simple behaviour can be expected, but verification of status
   codes, reason phrases, etc. can be verified, along with the correct set of
   responses allowed, etc. *)

module Defaults =

    let private defaults =
        freyaMachine {
            return () }

    [<Fact>]
    let ``default machine handles GET request appropriately`` () =
        let setup =
            Freya.empty

        verify setup defaults [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

    [<Fact>]
    let ``default machine handles HEAD request appropriately`` () =
        let setup =
            Request.method_ .= HEAD

        verify setup defaults [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK" ]

    [<Fact>]
    let ``default machine handles OPTIONS request appropriately`` () =
        let setup =
            Request.method_ .= OPTIONS

        verify setup defaults [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "Options" ]

    [<Fact>]
    let ``default machine handles POST request appropriately`` () =
        let setup =
            Request.method_ .= POST

        verify setup defaults [
            Response.statusCode_ => Some 405
            Response.reasonPhrase_ => Some "Method Not Allowed" ]
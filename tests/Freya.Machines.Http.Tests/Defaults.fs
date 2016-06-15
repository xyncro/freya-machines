module Freya.Machines.Http.Tests.Defaults

open Freya.Core.Operators
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Defaults

   Verification of default behaviour of an unconfigured Freya HTTP machine.
   Only relatively simple behaviour can be expected, but verification of status
   codes, reason phrases, etc. can be verified, along with the correct set of
   responses allowed, etc. *)

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

    verify setup defaultMachine [
        Response.statusCode_ => Some 405
        Response.reasonPhrase_ => Some "Method Not Allowed"
        Response.Headers.allow_ => Some (Allow [ HEAD; GET; OPTIONS ]) ]
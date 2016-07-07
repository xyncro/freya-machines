module Freya.Machines.Http.Cors.Tests.Simple

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Machines.Http.Cors
open Freya.Optics.Http.Cors
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http.Cors
open Xunit

(* Defaults *)

let defaultSetup =
        (Request.Headers.origin_ .= Some (Origin (OriginListOrNull.Origins [ Xyncro.com ])))

(* Simple

   Verification that CORS Simple requests are handled correctly given
   appropriate request data. *)

[<Fact>]
let ``basic cors allows request correctly`` () =

    let machine =
        freyaHttpMachine {
            cors }

    verify defaultSetup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowCredentials_ => Some (AccessControlAllowCredentials) ]

[<Fact>]
let ``cors disabled ignores cors`` () =

    let machine =
        freyaHttpMachine {
            cors
            corsEnabled false }

    verify defaultSetup machine [
        Response.Headers.accessControlAllowOrigin_ => None
        Response.Headers.accessControlAllowCredentials_ => None ]

[<Fact>]
let ``credentials unsupported cors behaves correctly`` () =

    (* Credentials Unsupported, Origins Unspecified *)

    let machine =
        freyaHttpMachine {
            cors
            corsSupportsCredentials false }

    verify defaultSetup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Any))
        Response.Headers.accessControlAllowCredentials_ => None ]

    (* Credentials Unsupported, Origins Empty *)

    let machine =
        freyaHttpMachine {
            cors
            corsOrigins []
            corsSupportsCredentials false }

    verify defaultSetup machine [
        Response.Headers.accessControlAllowOrigin_ => None
        Response.Headers.accessControlAllowCredentials_ => None ]

    (* Credentials Unsupported, Origins Restricted *)

    let machine =
        freyaHttpMachine {
            cors
            corsOrigins [ Xyncro.com ]
            corsSupportsCredentials false }

    verify defaultSetup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowCredentials_ => None ]

[<Fact>]
let ``headers exposed behaves correctly`` () =

    (* Exposed Headers Unspecified *)

    let machine =
        freyaHttpMachine {
            cors }

    verify defaultSetup machine [
        Response.Headers.accessControlExposeHeaders_ => None ]

    (* Exposed Headers Empty *)

    let machine =
        freyaHttpMachine {
            cors
            corsExposedHeaders [] }

    verify defaultSetup machine [
        Response.Headers.accessControlExposeHeaders_ => None ]

    (* Exposed Headers Non-Empty *)

    let machine =
        freyaHttpMachine {
            cors
            corsExposedHeaders [ "Server" ] }

    verify defaultSetup machine [
        Response.Headers.accessControlExposeHeaders_ => Some (AccessControlExposeHeaders ([ "Server" ])) ]
module Freya.Machines.Http.Cors.Tests.Simple

open Arachne.Http.Cors
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Machines.Http.Cors
open Freya.Optics.Http.Cors
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Simple

   Verification that the Assertions specifications behave as expected given
   suitable input. *)

[<Fact>]
let ``basic cors allows request correctly`` () =

    let machine =
        freyaHttpMachine {
            using cors }

    let setup =
        Request.Headers.origin_ .=
            Some (Origin (OriginListOrNull.Origins [ Xyncro.com ]))

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowCredentials_ => Some (AccessControlAllowCredentials) ]

[<Fact>]
let ``cors disabled ignores cors`` () =

    let machine =
        freyaHttpMachine {
            using cors
            corsEnabled false }

    let setup =
        Request.Headers.origin_ .=
            Some (Origin (OriginListOrNull.Origins [ Xyncro.com ]))

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => None
        Response.Headers.accessControlAllowCredentials_ => None ]

[<Fact>]
let ``credentials unsupported cors behaves correctly`` () =

    let setup =
        Request.Headers.origin_ .=
            Some (Origin (OriginListOrNull.Origins [ Xyncro.com ]))

    (* Credentials Unsupported, Origins Unspecified *)

    let machine =
        freyaHttpMachine {
            using cors
            corsSupportsCredentials false }

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Any))
        Response.Headers.accessControlAllowCredentials_ => None ]

    (* Credentials Unsupported, Origins Empty *)

    let machine =
        freyaHttpMachine {
            using cors
            corsOrigins []
            corsSupportsCredentials false }

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => None
        Response.Headers.accessControlAllowCredentials_ => None ]

    (* Credentials Unsupported, Origins Restricted *)

    let machine =
        freyaHttpMachine {
            using cors
            corsOrigins [ Xyncro.com ]
            corsSupportsCredentials false }

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowCredentials_ => None ]

[<Fact>]
let ``headers exposed behaves correctly`` () =

    let setup =
        Request.Headers.origin_ .=
            Some (Origin (OriginListOrNull.Origins [ Xyncro.com ]))

    (* Exposed Headers Unspecified *)

    let machine =
        freyaHttpMachine {
            using cors }

    verify setup machine [
        Response.Headers.accessControlExposeHeaders_ => None ]

    (* Exposed Headers Empty *)

    let machine =
        freyaHttpMachine {
            using cors
            corsExposedHeaders [] }

    verify setup machine [
        Response.Headers.accessControlExposeHeaders_ => None ]

    (* Exposed Headers Non-Empty *)

    let machine =
        freyaHttpMachine {
            using cors
            corsExposedHeaders [ "Server" ] }

    verify setup machine [
        Response.Headers.accessControlExposeHeaders_ => Some (AccessControlExposeHeaders ([ "Server" ])) ]
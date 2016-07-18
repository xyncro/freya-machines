module Freya.Machines.Http.Cors.Tests.Simple

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Machines.Http.Patch
open Freya.Optics.Http
open Freya.Optics.Http.Patch
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Freya.Types.Http.Patch
open Xunit

(* Defaults *)

let defaultSetup =
        (Request.method_ .= PATCH)
     *> (Request.Headers.contentLength_ .= Some (ContentLength 0))

(* Main

   Verification that PATCH support behaves broadly correctly. *)

[<Fact>]
let ``patch allows request correctly`` () =

    let machine =
        freyaHttpMachine {
            methods PATCH

            patch }

    verify defaultSetup machine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

[<Fact>]
let ``patch disabled ignores patch`` () =

    let machine =
        freyaHttpMachine {
            methods PATCH

            patch
            patchEnabled false }

    verify defaultSetup machine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

(* Headers

   Verification that the Accept-Patch header is set appropriately on OPTIONS
   requests. *)

[<Fact>]
let ``accept-patch header set appropriately`` () =

    let setup =
            Request.method_ .= OPTIONS

    (* Patch supported, no Media Types specified. *)

    let machine =
        freyaHttpMachine {
            methods [ OPTIONS; PATCH ]

            patch }
//            patchAcceptableMediaTypes MediaType.Json }

    verify setup machine [
        Response.statusCode_ => Some 200
        Response.Headers.acceptPatch_ => None ]

//    (* Credentials Unsupported, Origins Empty *)
//
//    let machine =
//        freyaHttpMachine {
//            cors
//            corsOrigins []
//            corsSupportsCredentials false }
//
//    verify defaultSetup machine [
//        Response.Headers.accessControlAllowOrigin_ => None
//        Response.Headers.accessControlAllowCredentials_ => None ]
//
//    (* Credentials Unsupported, Origins Restricted *)
//
//    let machine =
//        freyaHttpMachine {
//            cors
//            corsOrigins [ Xyncro.com ]
//            corsSupportsCredentials false }
//
//    verify defaultSetup machine [
//        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
//        Response.Headers.accessControlAllowCredentials_ => None ]
//
//[<Fact>]
//let ``headers exposed behaves correctly`` () =
//
//    (* Exposed Headers Unspecified *)
//
//    let machine =
//        freyaHttpMachine {
//            cors }
//
//    verify defaultSetup machine [
//        Response.Headers.accessControlExposeHeaders_ => None ]
//
//    (* Exposed Headers Empty *)
//
//    let machine =
//        freyaHttpMachine {
//            cors
//            corsExposedHeaders [] }
//
//    verify defaultSetup machine [
//        Response.Headers.accessControlExposeHeaders_ => None ]
//
//    (* Exposed Headers Non-Empty *)
//
//    let machine =
//        freyaHttpMachine {
//            cors
//            corsExposedHeaders [ "Server" ] }
//
//    verify defaultSetup machine [
//        Response.Headers.accessControlExposeHeaders_ => Some (AccessControlExposeHeaders ([ "Server" ])) ]
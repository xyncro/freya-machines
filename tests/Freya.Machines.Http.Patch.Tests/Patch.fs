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

    verify setup machine [
        Response.statusCode_ => Some 200
        Response.Headers.acceptPatch_ => None ]

    (* Patch supported, Media Types specified. *)

    let machine =
        freyaHttpMachine {
            methods [ OPTIONS; PATCH ]

            patch
            patchAcceptableMediaTypes MediaType.Json }

    verify setup machine [
        Response.statusCode_ => Some 200
        Response.Headers.acceptPatch_ => Some (AcceptPatch [ MediaType.Json ]) ]
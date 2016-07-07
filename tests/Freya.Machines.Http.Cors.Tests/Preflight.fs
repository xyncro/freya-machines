module Freya.Machines.Http.Cors.Tests.Preflight

open System
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Machines.Http.Cors
open Freya.Optics.Http
open Freya.Optics.Http.Cors
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Freya.Types.Http.Cors
open Xunit

(* Defaults *)

let defaultSetup =
        (Request.method_ .= OPTIONS)
     *> (Request.Headers.accessControlRequestMethod_ .= Some (AccessControlRequestMethod GET))
     *> (Request.Headers.origin_ .= Some (Origin (OriginListOrNull.Origins [ Xyncro.com ])))

(* Preflight

   Verification that CORS Preflight requests are handled correctly when given
   appropriate request data. *)

[<Fact>]
let ``basic cors allows request correctly`` () =

    let machine =
        freyaHttpMachine {
            cors }

    let setup =
            (defaultSetup)
         *> (Request.Headers.accessControlRequestHeaders_ .= Some (AccessControlRequestHeaders [ "Server" ]))

    verify setup machine [
        Response.Headers.accessControlAllowCredentials_ => Some (AccessControlAllowCredentials)
        Response.Headers.accessControlAllowHeaders_ => Some (AccessControlAllowHeaders [ "Server" ])
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowMethods_ => Some (AccessControlAllowMethods [ GET ])
        Response.Headers.accessControlMaxAge_ => None ]

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
let ``headers behaves correctly`` () =

    let setup =
            (defaultSetup)
         *> (Request.Headers.accessControlRequestHeaders_ .= Some (AccessControlRequestHeaders [ "Server" ]))

    (* Exposed Headers Unspecified *)

    let machine =
        freyaHttpMachine {
            cors }

    verify setup machine [
        Response.Headers.accessControlAllowHeaders_ => Some (AccessControlAllowHeaders ([ "Server" ])) ]

    (* Exposed Headers Empty *)

    let machine =
        freyaHttpMachine {
            cors
            corsHeaders [] }

    verify setup machine [
        Response.Headers.accessControlAllowHeaders_ => None ]

    (* Exposed Headers Non-Empty *)

    let machine =
        freyaHttpMachine {
            cors
            corsHeaders [ "Server" ] }

    verify setup machine [
        Response.Headers.accessControlAllowHeaders_ => Some (AccessControlAllowHeaders ([ "Server" ])) ]

[<Fact>]
let ``max age behaves correctly`` () =

    (* No Max Age *)

    let machine =
        freyaHttpMachine {
            cors }

    verify defaultSetup machine [
        Response.Headers.accessControlMaxAge_ => None ]

    (* Max Age*)

    let machine =
        freyaHttpMachine {
            cors
            corsMaxAge 3600 }

    verify defaultSetup machine [
        Response.Headers.accessControlMaxAge_ => Some (AccessControlMaxAge (TimeSpan.FromSeconds 3600.)) ]
module Freya.Machines.Http.Cors.Tests.Preflight

open Arachne.Http
open Arachne.Http.Cors
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Machines.Http.Cors
open Freya.Optics.Http
open Freya.Optics.Http.Cors
open Freya.Testing
open Freya.Testing.Operators
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
            using cors }

    let setup =
            (defaultSetup)
         *> (Request.Headers.accessControlRequestHeaders_ .= Some (AccessControlRequestHeaders [ "Server" ]))

    verify setup machine [
        Response.Headers.accessControlAllowCredentials_ => Some (AccessControlAllowCredentials)
        Response.Headers.accessControlAllowHeaders_ => Some (AccessControlAllowHeaders [ "Server" ])
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowMethods_ => Some (AccessControlAllowMethods [ GET ])
        Response.Headers.accessControlMaxAge_ => None ]
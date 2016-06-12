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

(* Preflight

   Verification that CORS Preflight requests are handled correctly when given
   appropriate request data. *)

[<Fact>]
let ``basic cors allows request correctly`` () =

    let machine =
        freyaHttpMachine {
            using cors }

    let setup =
            (Request.method_ .= OPTIONS)
         *> (Request.Headers.accessControlRequestMethod_ .= Some (AccessControlRequestMethod GET))
         *> (Request.Headers.origin_ .= Some (Origin (OriginListOrNull.Origins [ Xyncro.com ])))

    // TODO: verify headers allowed when implemented

    verify setup machine [
        Response.Headers.accessControlAllowOrigin_ => Some (AccessControlAllowOrigin (Origins (OriginListOrNull.Origins [ Xyncro.com ])))
        Response.Headers.accessControlAllowCredentials_ => Some (AccessControlAllowCredentials) ]
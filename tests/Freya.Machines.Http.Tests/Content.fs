module Freya.Machines.Http.Tests.Content

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Content

   Verification that the Content element behaves as expected given suitable
   input. *)

(* Content-Length *)

[<Fact>]
let ``machine handles absent content length correctly`` () =

    let setup =
            (Request.method_ .= POST)

    (* Content-Length *)

    let machine =
        freyaHttpMachine {
            methods POST }

    verify setup machine [
        Response.statusCode_ => Some 411
        Response.reasonPhrase_ => Some "Length Required" ]

(* Content-Type *)

[<Fact>]
let ``machine handles unsupported content type correctly`` () =

    let setup =
            (Request.method_ .= POST)
         *> (Request.Headers.contentLength_ .= Some (ContentLength 0))
         *> (Request.Headers.contentType_ .= Some (ContentType (MediaType.Text)))

    (* Content-Length *)

    let machine =
        freyaHttpMachine {
            acceptableMediaTypes MediaType.Json
            methods POST }

    verify setup machine [
        Response.statusCode_ => Some 415
        Response.reasonPhrase_ => Some "Unsupported Media Type" ]
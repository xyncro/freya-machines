module Freya.Machines.Http.Tests.Fallback

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Fallback

   Verification that the Fallback specification behaves correctly given
   appropriate input. *)

[<Fact>]
let ``machine invokes handleFallback correctly`` () =

    (* Fallback *)

    let FOO =
        Method.Custom "FOO"

    let setup =
        Request.method_ .= FOO

    let machine =
        freyaHttpMachine {
            methods FOO
            handleFallback ((defaultValue .= Some "Fallback") *> defaultRepresentation) }

    verify setup machine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK"
        defaultValue => Some "Fallback" ]
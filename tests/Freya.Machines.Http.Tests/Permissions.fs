module Freya.Machines.Http.Tests.Permissions

open Freya.Core
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Xunit

(* Permission

   Verification that the Permission element behaves as expected given suitable
   input. *)

(* Authorized *)

[<Fact>]
let ``machine handles authorized correctly`` () =

    (* Static *)

    let staticMachine =
        freyaMachine {
            authorized false }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 401
        Response.reasonPhrase_ => Some "Unauthorized" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/authorized"

    let dynamicMachine =
        freyaMachine {
            authorized ((=) "/authorized" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 401
        Response.reasonPhrase_ => Some "Unauthorized" ]

(* Allowed *)

[<Fact>]
let ``machine handles allowed correctly`` () =

    (* Static *)

    let staticMachine =
        freyaMachine {
            allowed false }

    verify defaultSetup staticMachine [
        Response.statusCode_ => Some 403
        Response.reasonPhrase_ => Some "Forbidden" ]

    (* Dynamic *)

    let setup =
        Request.path_ .= "/allowed"

    let dynamicMachine =
        freyaMachine {
            allowed ((=) "/allowed" <!> !. Request.path_) }

    verify setup dynamicMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    verify defaultSetup dynamicMachine [
        Response.statusCode_ => Some 403
        Response.reasonPhrase_ => Some "Forbidden" ]
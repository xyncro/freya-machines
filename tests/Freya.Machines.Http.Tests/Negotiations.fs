module Freya.Machines.Http.Tests.Negotiations

open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Freya.Types.Language
open Xunit

(* Negotiation

   Verification that the Negotiations specifications behave as expected given
   suitable input. The neogtiation block may return 412 for any failure to
   negotiate. *)

(* Accept *)

[<Fact>]
let ``machine handles accept negotiation correctly`` () =

    let mediaRange =
        Closed (Type "text", SubType "plain", Parameters Map.empty)

    let acceptParameters =
        AcceptParameters (Weight 1., Extensions Map.empty)

    let accept =
        Accept [ AcceptableMedia (mediaRange, Some acceptParameters) ]

    let setup =
        Request.Headers.accept_ .= Some accept

    (* Unconfigured *)

    verify setup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Configured *)

    let matchedMachine =
        freyaHttpMachine {
            mediaTypesSupported (MediaType (Type "text", SubType "plain", Parameters Map.empty)) }

    verify setup matchedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    let unmatchedMachine =
        freyaHttpMachine {
            mediaTypesSupported (MediaType (Type "application", SubType "json", Parameters Map.empty)) }

    verify setup unmatchedMachine [
        Response.statusCode_ => Some 406
        Response.reasonPhrase_ => Some "Not Acceptable" ]

(* Accept-Language *)

[<Fact>]
let ``machine handles accept-language negotiation correctly`` () =

    let acceptLanguage =
        AcceptLanguage [ AcceptableLanguage (Range [ "en" ], None) ]

    let setup =
        Request.Headers.acceptLanguage_ .= Some acceptLanguage

    (* Unconfigured *)

    verify setup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Configured *)

    let matchedMachine =
        freyaHttpMachine {
            languagesSupported (LanguageTag (Language ("en", None), None, None, Variant [])) }

    verify setup matchedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    let unmatchedMachine =
        freyaHttpMachine {
            languagesSupported (LanguageTag (Language ("de", None), None, None, Variant [])) }

    verify setup unmatchedMachine [
        Response.statusCode_ => Some 406
        Response.reasonPhrase_ => Some "Not Acceptable" ]

(* Accept-Charset *)

[<Fact>]
let ``machine handles accept-charset negotiation correctly`` () =

    let acceptCharset =
        AcceptCharset [ AcceptableCharset (CharsetRange.Charset (Charset "utf-8"), None) ]

    let setup =
        Request.Headers.acceptCharset_ .= Some acceptCharset

    (* Unconfigured *)

    verify setup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Configured *)

    let matchedMachine =
        freyaHttpMachine {
            charsetsSupported (Charset "utf-8") }

    verify setup matchedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    let unmatchedMachine =
        freyaHttpMachine {
            charsetsSupported (Charset "iso-8859-1") }

    verify setup unmatchedMachine [
        Response.statusCode_ => Some 406
        Response.reasonPhrase_ => Some "Not Acceptable" ]

(* Accept-Encoding *)

[<Fact>]
let ``machine handles accept-encoding negotiation correctly`` () =

    let acceptEncoding =
        AcceptEncoding [ AcceptableEncoding (Coding (ContentCoding "gzip"), None) ]

    let setup =
        Request.Headers.acceptEncoding_ .= Some acceptEncoding

    (* Unconfigured *)

    verify setup defaultMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    (* Configured *)

    let matchedMachine =
        freyaHttpMachine {
            contentCodingsSupported (ContentCoding "gzip") }

    verify setup matchedMachine [
        Response.statusCode_ => Some 200
        Response.reasonPhrase_ => Some "OK" ]

    let unmatchedMachine =
        freyaHttpMachine {
            contentCodingsSupported (ContentCoding "compress") }

    verify setup unmatchedMachine [
        Response.statusCode_ => Some 406
        Response.reasonPhrase_ => Some "Not Acceptable" ]
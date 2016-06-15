module Freya.Machines.Http.Tests.Preconditions

open System
open Freya.Core.Operators
open Freya.Machines.Http
open Freya.Optics.Http
open Freya.Testing
open Freya.Testing.Operators
open Freya.Types.Http
open Xunit

(* Preconditions

   Verification that the various Preconditions specifications behave as
   expected. *)

(* Common *)

module Common =

    (* If-Match *)

    [<Fact>]
    let ``machine handles if-match correctly`` () =

        let anySetup =
            Request.Headers.ifMatch_ .= Some (IfMatch (IfMatchChoice.Any))

        let matchedSetup =
            Request.Headers.ifMatch_ .= Some (IfMatch (IfMatchChoice.EntityTags [ Strong "foo" ]))

        let unmatchedSetup =
            Request.Headers.ifMatch_ .= Some (IfMatch (IfMatchChoice.EntityTags [ Strong "bar" ]))

        let machine =
            freyaHttpMachine {
                entityTag (Strong "foo") }

        verify anySetup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => None ]

        verify anySetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]

        verify matchedSetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]

        verify unmatchedSetup machine [
            Response.statusCode_ => Some 412
            Response.reasonPhrase_ => Some "Precondition Failed"
            Response.Headers.eTag_ => None ]

    (* If-Unmodified-Since *)

    [<Fact>]
    let ``machine handles if-unmodified-since correctly`` () =

        let baseDate =
            DateTime (2000, 1, 1)

        let newSetup =
            Request.Headers.ifUnmodifiedSince_ .= Some (IfUnmodifiedSince (baseDate))

        let oldSetup =
            Request.Headers.ifUnmodifiedSince_ .= Some (IfUnmodifiedSince (baseDate.AddDays (-2.)))

        let machine =
            freyaHttpMachine {
                lastModified (baseDate.AddDays (-1.)) }

        verify newSetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.lastModified_ => Some (LastModified (baseDate.AddDays (-1.))) ]

        verify oldSetup machine [
            Response.statusCode_ => Some 412
            Response.reasonPhrase_ => Some "Precondition Failed" ]

(* Safe *)

module Safe =

    (* If-None-Match *)

    [<Fact>]
    let ``machine handles if-none-match correctly`` () =

        let anySetup =
            Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.Any))

        let matchedSetup =
            Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.EntityTags [ Weak "foo" ]))

        let unmatchedSetup =
            Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.EntityTags [ Weak "bar" ]))

        let machine =
            freyaHttpMachine {
                entityTag (Strong "foo") }

        verify anySetup defaultMachine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => None ]

        verify anySetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]

        verify matchedSetup machine [
            Response.statusCode_ => Some 304
            Response.reasonPhrase_ => Some "Not Modified"
            Response.Headers.eTag_ => None ]

        verify unmatchedSetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]

    (* If-Modified-Since *)

    [<Fact>]
    let ``machine handles if-modified-since correctly`` () =

        let baseDate =
            DateTime (2000, 1, 1)

        let newSetup =
            Request.Headers.ifModifiedSince_ .= Some (IfModifiedSince (baseDate))

        let oldSetup =
            Request.Headers.ifModifiedSince_ .= Some (IfModifiedSince (baseDate.AddDays (-2.)))

        let machine =
            freyaHttpMachine {
                lastModified (baseDate.AddDays (-1.)) }

        verify newSetup machine [
            Response.statusCode_ => Some 304
            Response.reasonPhrase_ => Some "Not Modified" ]

        verify oldSetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.lastModified_ => Some (LastModified (baseDate.AddDays (-1.))) ]

(* Unsafe *)

module Unsafe =

    (* If-None-Match *)

    [<Fact>]
    let ``machine handles if-none-match correctly`` () =

        let anySetup =
                (Request.method_ .= POST)
             *> (Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.Any)))

        let matchedSetup =
                (Request.method_ .= POST)
             *> (Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.EntityTags [ Weak "foo" ])))

        let unmatchedSetup =
                (Request.method_ .= POST)
             *> (Request.Headers.ifNoneMatch_ .= Some (IfNoneMatch (IfNoneMatchChoice.EntityTags [ Weak "bar" ])))

        let machine =
            freyaHttpMachine {
                methods POST
                entityTag (Strong "foo") }

        verify anySetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]

        verify matchedSetup machine [
            Response.statusCode_ => Some 412
            Response.reasonPhrase_ => Some "Precondition Failed" ]

        verify unmatchedSetup machine [
            Response.statusCode_ => Some 200
            Response.reasonPhrase_ => Some "OK"
            Response.Headers.eTag_ => Some (ETag (Strong "foo")) ]
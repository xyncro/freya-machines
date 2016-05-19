namespace Freya.Machines.Http

open System
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http

(* Operation

   Common operations for standard HTTP responses, setting various header values
   according to the appropriate logic for the response. These are commonly used
   by various terminals within the HTTP Machine.

   Operations are made available at the top level as they are generally useful
   when implementing lower level abstractions but using the Freya stack - thus
   they are made available "as a service"! *)

[<RequireQualifiedAccess>]
module Operation =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member Operation (x: Freya<bool>) =
                x

            static member Operation (x: Freya<unit>) =
                Freya.map (x, fun _ -> true)

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Operation: ^a -> Freya<bool>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

    (* Setters *)

    let private allow =
        function | methods -> Response.Headers.allow_ .= Some (Allow (Set.toList methods))

    let private date =
        function | () -> Response.Headers.date_ .= Some (Date.Date (DateTime.UtcNow))

    let private eTag =
        function | Some entityTag -> Response.Headers.eTag_ .= Some (ETag entityTag)
                 | _ -> Freya.empty

    let private lastModified =
        function | Some dateTime -> Response.Headers.lastModified_ .= Some (LastModified dateTime)
                 | _ -> Freya.empty

    let private phrase =
        function | phrase -> Response.reasonPhrase_ .= Some phrase

    let private status =
        function | status -> Response.statusCode_ .= Some status

    (* 2xx *)

    let ok entityTag modified =
            status 200
         *> phrase "OK"
         *> eTag entityTag
         *> lastModified modified
         *> date ()


    let options =
            status 200
         *> phrase "Options"
         *> date ()

    let created =
            status 201
         *> phrase "Created"
         *> date ()

    let accepted =
            status 202
         *> phrase "Accepted"
         *> date ()

    let noContent =
            status 204
         *> phrase "No Content"
         *> date ()

    (* 3xx *)

    let multipleChoices =
            status 300
         *> phrase "Multiple Choices"
         *> date ()

    let movedPermanently =
            status 301
         *> phrase "Moved Permanently"
         *> date ()

    let found =
            status 302
         *> phrase "Found"
         *> date ()

    let seeOther =
            status 303
         *> phrase "See Other"
         *> date ()

    let notModified =
            status 304
         *> phrase "Not Modified"
         *> date ()

    let temporaryRedirect =
            status 307
         *> phrase "Temporary Redirect"
         *> date ()

    (* 4xx *)

    let badRequest =
            status 400
         *> phrase "Bad Request"
         *> date ()

    let unauthorized =
            status 401
         *> phrase "Unauthorized"
         *> date ()

    let forbidden =
            status 403
         *> phrase "Forbidden"
         *> date ()

    let notFound =
            status 404
         *> phrase "Not Found"
         *> date ()

    let methodNotAllowed allowed =
            status 405
         *> phrase "Method Not Allowed"
         *> date ()
         *> allow allowed

    let notAcceptable =
            status 406
         *> phrase "Not Acceptable"
         *> date ()

    let conflict =
            status 409
         *> phrase "Conflict"
         *> date ()

    let gone =
            status 410
         *> phrase "Gone"
         *> date ()

    let preconditionFailed =
            status 412
         *> phrase "Precondition Failed"
         *> date ()

    let uriTooLong =
            status 414
         *> phrase "URI Too Long"
         *> date ()

    let expectationFailed =
            status 417
         *> phrase "Expectation Failed"
         *> date ()

    (* 5xx *)

    let internalServerError =
            status 500
         *> phrase "Internal Server Error"
         *> date ()

    let notImplemented =
            status 501
         *> phrase "Not Implemented"
         *> date ()

    let serviceUnavailable =
            status 503
         *> phrase "Service Unavailable"
         *> date ()

    let httpVersionNotSupported =
            status 505
         *> phrase "HTTP Version Not Supported"
         *> date ()

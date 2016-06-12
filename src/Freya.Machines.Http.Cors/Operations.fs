namespace Freya.Machines.Http.Cors

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http.Cors

(* Operations *)

[<RequireQualifiedAccess>]
module Operations =

    (* Setters *)

    let private accessControlAllowOrigin =
        function | x -> Response.Headers.accessControlAllowOrigin_ .= Some (AccessControlAllowOrigin x)

    let private accessControlExposeHeaders =
        function | x -> Response.Headers.accessControlExposeHeaders_ .= Some (AccessControlExposeHeaders x)

    let private accessControlSupportsCredentials =
        function | _ -> Response.Headers.accessControlAllowCredentials_ .= Some (AccessControlAllowCredentials)

    (* Helpers

       Helper functions for setting dependent groups of headers based on logic
       within the CORS specification. *)

    (* Allow Origin and Supports Credentials

       Logic for correctly setting the origin and credential support response
       headers, based on whether credentials are supported, and on whether the
       list of origins has been defined ("any" is assumed where valid if not). *)

    let rec allowOriginAndSupportsCredentials =
        function | Some (Origin origin) -> allowOriginAndSupportsCredentialsHeaders origin
                 | _ -> empty

    and private allowOriginAndSupportsCredentialsHeaders origin =
        function | false, None -> accessControlAllowOrigin Any
                 | false, _ -> accessControlAllowOrigin (Origins origin)
                 | true, _ -> accessControlSupportsCredentials () *> accessControlAllowOrigin (Origins origin)

    and private empty =
        function | _ -> Freya.empty

    (* Headers

       Logic for setting headers supported and exposed by a resource. *)

    let exposeHeaders =
        function | Some x when not (Set.isEmpty x) -> accessControlExposeHeaders (Set.toList x)
                 | _ -> Freya.empty
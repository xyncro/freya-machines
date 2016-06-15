namespace Freya.Machines.Http.Cors

#nowarn "46"

open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http.Cors
open Freya.Types.Http.Cors

(* Operations *)

[<RequireQualifiedAccess>]
module Operations =

    (* Setters

       Common setter functions for basic headers, given pre-formed input data. *)

    let private accessControlAllowHeaders =
        function | x -> Response.Headers.accessControlAllowHeaders_ .= Some (AccessControlAllowHeaders x)

    let private accessControlAllowMethods =
        function | x -> Response.Headers.accessControlAllowMethods_ .= Some (AccessControlAllowMethods x)

    let private accessControlAllowOrigin =
        function | x -> Response.Headers.accessControlAllowOrigin_ .= Some (AccessControlAllowOrigin x)

    let private accessControlExposeHeaders =
        function | x -> Response.Headers.accessControlExposeHeaders_ .= Some (AccessControlExposeHeaders x)

    let private accessControlMaxAge =
        function | x -> Response.Headers.accessControlMaxAge_ .= Some (AccessControlMaxAge x)

    let private accessControlSupportsCredentials =
        function | _ -> Response.Headers.accessControlAllowCredentials_ .= Some (AccessControlAllowCredentials)

    (* Operations

       Operations for setting CORS related response headers, including logic
       to determine permutation when the input data is potentially of varying
       shapes, and may have dependent logic. *)

    (* Allow Headers *)

    let rec allowHeaders headers =
        allowHeadersPermutations headers

    and private allowHeadersPermutations =
        function | Some (AccessControlRequestHeaders x) -> accessControlAllowHeaders x
                 | _ -> Freya.empty

    (* Allow Methods *)

    let rec allowMethods method =
        allowMethodsPermutations method

    and private allowMethodsPermutations =
        function | Some (AccessControlRequestMethod x) -> accessControlAllowMethods [ x ]
                 | _ -> Freya.empty

    (* Allow Origin and Supports Credentials *)

    let rec allowOriginAndSupportsCredentials origin supportsCredentials origins =
        allowOriginAndSupportsCredentialsPermutations origin (supportsCredentials, origins)

    and private allowOriginAndSupportsCredentialsPermutations =
        function | Some (Origin x) -> allowOriginAndSupportsCredentialsHeaders x
                 | _ -> empty

    and private allowOriginAndSupportsCredentialsHeaders origin =
        function | false, None -> accessControlAllowOrigin Any
                 | false, _ -> accessControlAllowOrigin (Origins origin)
                 | true, _ -> accessControlSupportsCredentials () *> accessControlAllowOrigin (Origins origin)

    and private empty =
        function | _ -> Freya.empty

    (* Expose Headers *)

    let rec exposeHeaders headers =
        exposeHeadersPermutations headers

    and private exposeHeadersPermutations =
        function | Some x when not (Set.isEmpty x) -> accessControlExposeHeaders (Set.toList x)
                 | _ -> Freya.empty

    (* Max Age *)

    let rec maxAge age =
        maxAgePermutations age

    and private maxAgePermutations =
        function | Some x -> accessControlMaxAge x
                 | _ -> Freya.empty
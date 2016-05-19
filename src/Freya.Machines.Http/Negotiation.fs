namespace Freya.Machines.Http

open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http

(* Negotiation

   Available and Acceptable types, representing the available charsets,
   encodings etc. defined for a resource (where defined) and the acceptable
   values from the available set (where defined) given a request. When the
   request does not specify criteria, or where no values are offered as part of
   the availablity data, the negotiation is defined to be Free, and at the
   discretion of the implementation. *)

type Acceptance<'a> =
    | Acceptable of 'a list
    | Free

(* Negotiation

   Functions for determining the negotiable values of a representation given
   defined available values. Each negotiator function returns a function which
   will negotiate the relevant aspect of the request. *)

[<RequireQualifiedAccess>]
module Negotiation =

    (* Negotiation *)

    let private negotiated =
        function | Some x -> Acceptable x
                 | _ -> Free

    let negotiable =
        function | Acceptable (_ :: _) | Free -> true
                 | _ -> false

    (* Charset *)

    [<RequireQualifiedAccess>]
    module Charset =

        let private negotiate supported =
            function | Some (AcceptCharset x) -> negotiated (Charset.negotiate supported (Some x))
                     | _ -> Free

        let negotiator =
            function | Some x -> negotiate x <!> !. Request.Headers.acceptCharset_
                     | _ -> Freya.init Free

    (* ContentCoding *)

    [<RequireQualifiedAccess>]
    module ContentCoding =

        let private negotiate supported =
            function | Some (AcceptEncoding x) -> negotiated (ContentCoding.negotiate supported (Some x))
                     | _ -> Free

        let negotiator =
            function | Some x -> negotiate x <!> !. Request.Headers.acceptEncoding_
                     | _ -> Freya.init Free

    (* Language *)

    [<RequireQualifiedAccess>]
    module Language =

        let private negotiate supported =
            function | Some (AcceptLanguage x) -> negotiated (Language.negotiate supported (Some x))
                     | _ -> Free

        let negotiator =
            function | Some x -> negotiate x <!> !. Request.Headers.acceptLanguage_
                     | _ -> Freya.init Free

    (* MediaType *)

    [<RequireQualifiedAccess>]
    module MediaType =

        let private negotiate supported =
            function | Some (Accept x) -> negotiated (MediaType.negotiate supported (Some x))
                     | _ -> Free

        let negotiator =
            function | Some x -> negotiate x <!> !. Request.Headers.accept_
                     | _ -> Freya.init Free
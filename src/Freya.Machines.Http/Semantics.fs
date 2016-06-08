namespace Freya.Machines.Http

open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Language
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
module internal Negotiation =

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

(* Representation

   Types defining the data and representation of a response to be returned to
   the client, where the representation (defined by the Description) type is
   likely a result of the negotiation data provided. *)

type Available =
    { Charsets: Charset list option
      Encodings: ContentCoding list option
      MediaTypes: MediaType list option
      Languages: LanguageTag list option }

 and Acceptable =
    { Charsets: Acceptance<Charset>
      Encodings: Acceptance<ContentCoding>
      MediaTypes: Acceptance<MediaType>
      Languages: Acceptance<LanguageTag> }

type Representation =
    { Data: byte []
      Description: Description }

    static member empty =
        { Data = [||]
          Description =
            { Charset = None
              Encodings = None
              MediaType = None
              Languages = None } }

 and Description =
    { Charset: Charset option
      Encodings: ContentCoding list option
      MediaType: MediaType option
      Languages: LanguageTag list option }

(* Representation

   Representation logic for determining the appropriate specification of a
   resource given the negotiation available, and writing a representation
   to the response given the data and a description of that data. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Representation =

    (* Optics *)

    let private charset_ =
            Response.Headers.contentType_
        >-> Option.mapIsomorphism ContentType.mediaType_
        >-> Option.mapLens MediaType.parameters_
        >?> Parameters.parameters_
        >?> Map.value_ "charset"

    (* Negotiation *)

    let private negotiate (available: Available) : Freya<Acceptable> =
            fun charsets encodings mediaTypes languages ->
                { Charsets = charsets
                  Encodings = encodings
                  MediaTypes = mediaTypes
                  Languages = languages }
        <!> Negotiation.Charset.negotiator available.Charsets
        <*> Negotiation.ContentCoding.negotiator available.Encodings
        <*> Negotiation.MediaType.negotiator available.MediaTypes
        <*> Negotiation.Language.negotiator available.Languages

    (* Representation *)

    let private body =
            function | data -> !. Request.method_
                              >>= function | HEAD -> Freya.empty
                                           | _ -> Response.body_ %= (fun x -> x.Write (data, 0, data.Length); x)

    let private charset =
            function | Some (Charset charset) -> charset_ .= Some charset
                     | _ -> Freya.empty

    let private encodings =
            function | Some encodings -> Response.Headers.contentEncoding_ .= Some (ContentEncoding encodings)
                     | _ -> Freya.empty

    let private languages =
            function | Some languages -> Response.Headers.contentLanguage_ .= Some (ContentLanguage languages)
                     | _ -> Freya.empty

    let private mediaType =
            function | Some mediaType -> Response.Headers.contentType_ .= Some (ContentType mediaType)
                     | _ -> Freya.empty

    let private write (r: Representation) =
            mediaType r.Description.MediaType
         *> charset r.Description.Charset
         *> encodings r.Description.Encodings
         *> languages r.Description.Languages
         *> body r.Data

    let represent available handler =
            available
        >>= negotiate
        >>= handler
        >>= write
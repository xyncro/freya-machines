namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http
open Freya.Machines.Http.Machine.Configuration
open Freya.Optics.Http

(* Negotiations

   Decisions determing whether a request can be negotiated, given the
   support for the varying types of negotiable properties either
   declared through configuration, or set as defaults.

   Failure of these checks will result in a 406 response, signalling a
   client error. *)

[<RequireQualifiedAccess>]
module Negotiations =

    (* Types *)

    type private Negotiations =
        { Terminals: Terminals }

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Terminals = Terminals.empty }

     and private Terminals =
        { NotAcceptable: Handler option }

        static member notAcceptable_ =
            (fun x -> x.NotAcceptable), (fun n x -> { x with NotAcceptable = n })

        static member empty =
            { NotAcceptable = None }

    (* Key *)

    let private key =
        Key.root >> Key.add [ "negotiations" ]

    (* Optics *)

    let private negotiations_ =
        Configuration.element_ Negotiations.empty [ "http"; "specifications"; "negotiations" ]

    (* Aliases

       Shorthand/abbreviations for common functions, used locally to make the
       code more concise where these verbose formulations make the logic harder
       to read. *)

    (* Monadic *)

    let private bind =
        Value.Freya.bind

    let private map =
        Value.Freya.map

    (* Negotiation *)

    let private charset =
        Negotiation.Charset.negotiator

    let private contentCoding =
        Negotiation.ContentCoding.negotiator

    let private language =
        Negotiation.Language.negotiator

    let private mediaType =
        Negotiation.MediaType.negotiator

    let private negotiable =
        Negotiation.negotiable

    (* Optics *)

    let private charsets_ =
        Properties.Representation.charsetsSupported_

    let private contentCodings_ =
        Properties.Representation.contentCodingsSupported_

    let private languages_ =
        Properties.Representation.languagesSupported_

    let private mediaTypes_ =
        Properties.Representation.mediaTypesSupported_

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                negotiations_
            >-> Negotiations.terminals_

        let notAcceptable_ =
                terminals_
            >-> Terminals.notAcceptable_

        let notAcceptable k =
            Terminal.create (key k, "not-acceptable")
                (function | _ -> Operations.notAcceptable)
                (function | Get notAcceptable_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let rec hasAccept k s =
            Decision.create (key k, "has-accept")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.accept_))
                (hasAcceptLanguage k s, acceptMatches k s)

        and acceptMatches k s =
            Decision.create (key k, "accept-matches")
                (function | TryGet mediaTypes_ x -> map negotiable (bind (Some >> mediaType) x)
                          | _ -> Static true)
                (Terminals.notAcceptable k, hasAcceptLanguage k s)

        and hasAcceptLanguage k s =
            Decision.create (key k, "has-accept-language")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.acceptLanguage_))
                (hasAcceptCharset k s, acceptLanguageMatches k s)

        and acceptLanguageMatches k s =
            Decision.create (key k, "accept-language-matches")
                (function | TryGet languages_ x -> map negotiable (bind (Some >> language) x)
                          | _ -> Static true)
                (Terminals.notAcceptable k, hasAcceptCharset k s)

        and hasAcceptCharset k s =
            Decision.create (key k, "has-accept-charset")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.acceptCharset_))
                (hasAcceptEncoding k s, acceptCharsetMatches k s)

        and acceptCharsetMatches k s =
            Decision.create (key k, "accept-charset-matches")
                (function | TryGet charsets_ x -> map negotiable (bind (Some >> charset) x)
                          | _ -> Static true)
                (Terminals.notAcceptable k, hasAcceptEncoding k s)

        and hasAcceptEncoding k s =
            Decision.create (key k, "has-accept-encoding")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.acceptEncoding_))
                (s, acceptEncodingMatches k s)

        and acceptEncodingMatches k s =
            Decision.create (key k, "accept-encoding-matches")
                (function | TryGet contentCodings_ x -> map negotiable (bind (Some >> contentCoding) x)
                          | _ -> Static true)
                (Terminals.notAcceptable k, s)

    (* Specification *)

    let specification =
        Decisions.hasAccept
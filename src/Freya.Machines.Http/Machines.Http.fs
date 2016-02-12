module Freya.Machines.Http

open System
open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Language
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http
open Hephaestus

// TODO: Defaults (???)
// TODO: Rest of Machine!
// TODO: Introduce a mechanism for logs, etc.

(* Negotiation *)

[<AutoOpen>]
module internal Negotiation =

    (* Types *)

    type Negotiation<'a> =
        | Negotiated of 'a list
        | Free

    (* Functions *)

    let private negotiated =
        function | Some x -> Negotiated x
                 | _ -> Free

    (* Charset *)

    [<RequireQualifiedAccess>]
    module Charset =

        let negotiate supported acceptCharset =
            Option.map (function | AcceptCharset x -> x) acceptCharset
            |> Charset.negotiate supported
            |> negotiated

        (* Decisions *)

        let negotiable supported acceptCharset =
            negotiate supported acceptCharset
            |> function | Negotiated x when not (List.isEmpty x) -> true
                        | _ -> false

    (* ContentCoding *)

    [<RequireQualifiedAccess>]
    module ContentCoding =

        let negotiate supported acceptEncoding =
            Option.map (function | AcceptEncoding x -> x) acceptEncoding
            |> ContentCoding.negotiate supported
            |> negotiated

        (* Decisions *)

        let negotiable supported acceptEncoding =
            negotiate supported acceptEncoding
            |> function | Negotiated x when not (List.isEmpty x) -> true
                        | _ -> false

    (* Language *)

    [<RequireQualifiedAccess>]
    module Language =

        let negotiate supported acceptLanguage =
            Option.map (function | AcceptLanguage x -> x) acceptLanguage
            |> Language.negotiate supported
            |> negotiated

        (* Decisions *)

        let negotiable supported acceptLanguage =
            negotiate supported acceptLanguage
            |> function | Negotiated x when not (List.isEmpty x) -> true
                        | _ -> false

    (* MediaType *)

    [<RequireQualifiedAccess>]
    module MediaType =

        let negotiate supported accept =
            Option.map (function | Accept x -> x) accept
            |> MediaType.negotiate supported
            |> negotiated

        (* Decisions *)

        let negotiable supported accept =
            negotiate supported accept
            |> function | Negotiated x when not (List.isEmpty x) -> true
                        | _ -> false

(* Operations

   Common operations for standard HTTP responses, setting various header values
   according to the appropriate logic for the response. These are commonly used
   by various terminals within the HTTP Machine.

   Operations are made available at the top level as they are generally useful
   when implementing lower level abstractions but using the Freya stack - thus
   they are made available "as a service"! *)

[<RequireQualifiedAccess>]
module Operations =

    let private date =
        Date.Date >> Some >> (.=) Response.Headers.date_ <| DateTime.UtcNow

    let private phrase =
        Some >> (.=) Response.reasonPhrase_

    let private status =
        Some >> (.=) Response.statusCode_

    (* 2xx *)

    let ok =
            status 200
         *> phrase "OK"
         *> date

    (* 4xx *)

    let badRequest =
            status 400
         *> phrase "Bad Request"
         *> date

    let unauthorized =
            status 401
         *> phrase "Unauthorized"
         *> date

    let forbidden =
            status 403
         *> phrase "Forbidden"
         *> date

    let methodNotAllowed =
            status 405
         *> phrase "Method Not Allowed"
         *> date

    let notAcceptable =
            status 406
         *> phrase "Not Acceptable"
         *> date

    let uriTooLong =
            status 414
         *> phrase "URI Too Long"
         *> date

    let expectationFailed =
            status 417
         *> phrase "Expectation Failed"
         *> date

    (* 5xx *)

    let notImplemented =
            status 501
         *> phrase "Not Implemented"
         *> date

    let serviceUnavailable =
            status 503
         *> phrase "Service Unavailable"
         *> date

    let httpVersionNotSupported =
            status 505
         *> phrase "HTTP Version Not Supported"
         *> date

(* Model

   A Hephaestus Model defining the semantics of HTTP execution, defining the
   HTTP request/response process as a series of decisions.

   Unlike other implementations of this model (initially adopted by WebMachine,
   et al.), the Freya implementation does not optimize the graph for lack of
   repetition and reuse, but for straight-line paths, in the knowledge that the
   overall graph will be optimized by the underlying Hephaestus engine. *)

[<RequireQualifiedAccess>]
module Model =

    [<AutoOpen>]
    module Prelude =

        (* Keys

           Functions for working with Hephaestus Keys, making defining and using
           keys slightly more pleasant. The default empty key is included here
           for consistency at the various levels. *)

        [<RequireQualifiedAccess>]
        module Key =

            let add x =
                Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) [ x ])

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let inline fromConfigurationOrStatic o s =
                function | TryGet o x -> x
                         | _ -> Static s

            let inline fromConfigurationOrTrue o =
                fromConfigurationOrStatic o true

            let inline fromConfigurationOrFalse o =
                fromConfigurationOrStatic o false

        (* Terminals *)

        [<RequireQualifiedAccess>]
        module Terminal =

            let inline fromConfigurationWithOperation o operation =
                function | TryGet o x -> x *> operation
                         | _ -> operation

        (* Specifications *)

        let internal decision (key, name) configurator (t, f) =
            Specification.Decision.create (Key.add name key) (configurator >> Decision.map) (t, f)

        let internal terminal (key, name) configurator =
            Specification.Terminal.create (Key.add name key) configurator

    (* Key *)

    let private key =
        Key.add "http" Key.empty

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key =
            Key.add "common" key

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module internal Configuration =

            (* Types *)

            type Common =
                { Properties: Properties
                  Terminals: Terminals }

                static member properties_ =
                    (fun x -> x.Properties), (fun p x -> { x with Properties = p })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Properties = Properties.empty
                      Terminals = Terminals.empty }

             and Properties =
                { MediaTypesSupported: Value<MediaType list> option
                  LanguagesSupported: Value<LanguageTag list> option
                  CharsetsSupported: Value<Charset list> option
                  ContentCodingsSupported: Value<ContentCoding list> option }

                static member mediaTypesSupported_ =
                    (fun x -> x.MediaTypesSupported), (fun m x -> { x with MediaTypesSupported = m })

                static member languagesSupported_ =
                    (fun x -> x.LanguagesSupported), (fun l x -> { x with LanguagesSupported = l })

                static member charsetsSupported_ =
                    (fun x -> x.CharsetsSupported), (fun c x -> { x with CharsetsSupported = c })

                static member contentCodingsSupported_ =
                    (fun x -> x.ContentCodingsSupported), (fun c x -> { x with ContentCodingsSupported = c })

                static member empty =
                    { MediaTypesSupported = None
                      LanguagesSupported = None
                      CharsetsSupported = None
                      ContentCodingsSupported = None }

             and Terminals =
                { Ok: Freya<unit> option }

                static member ok_ =
                    (fun x -> x.Ok), (fun o x -> { x with Ok = o })

                static member empty =
                    { Ok = None }

            (* Optics *)

            let common_ =
                Configuration.element_ Common.empty "common"

        (* Properties *)

        [<RequireQualifiedAccess>]
        module Properties =

            let private properties_ =
                    Configuration.common_
                >-> Configuration.Common.properties_

            let mediaTypesSupported_ =
                    properties_
                >-> Configuration.Properties.mediaTypesSupported_

            let languagesSupported_ =
                    properties_
                >-> Configuration.Properties.languagesSupported_

            let charsetsSupported_ =
                    properties_
                >-> Configuration.Properties.charsetsSupported_

            let contentCodingsSupported_ =
                    properties_
                >-> Configuration.Properties.contentCodingsSupported_

        (* Terminals *)

        [<RequireQualifiedAccess>]
        module Terminals =

            let private terminals_ =
                    Configuration.common_
                >-> Configuration.Common.terminals_

            let ok_ =
                    terminals_
                >-> Configuration.Terminals.ok_

            let internal ok =
                terminal (key, "ok-terminal")
                    (Terminal.fromConfigurationWithOperation ok_ Operations.ok)

    (* Components *)

    [<RequireQualifiedAccess>]
    module Components =

       // TODO: Consider whether fixed keys is appropriate here

        let private key =
            Key.add "components" key

        [<RequireQualifiedAccess>]
        module Common =

            let private key =
                Key.add "common" key

            [<RequireQualifiedAccess>]
            module internal Configuration =

                (* Types *)

                type Common =
                    { Existence: Existence }

                    static member existence_ =
                        (fun x -> x.Existence), (fun e x -> { x with Existence = e })

                    static member empty =
                        { Existence = Existence.empty }

                 and Existence =
                    { Exists: Value<bool> option }

                    static member exists_ =
                        (fun x -> x.Exists), (fun e x -> { x with Exists = e })

                    static member empty =
                        { Exists = None }

                (* Optics *)

                let common_ =
                    Configuration.element_ Common.empty "components.common"

            [<RequireQualifiedAccess>]
            module Method =

                let private key =
                    Key.add "method" key

                let internal methodMatches m (l, r) =
                    decision (key, "method-matches-decision")
                        (fun _ -> Dynamic ((=) m <!> !. Request.method_))
                        (l, r)

            [<RequireQualifiedAccess>]
            module Existence =

                let private key =
                    Key.add "existence" key

                let exists_ =
                        Configuration.common_
                    >-> Configuration.Common.existence_
                    >-> Configuration.Existence.exists_

                let internal exists (l, r) =
                    decision (key, "exists-decision")
                        (Decision.fromConfigurationOrTrue exists_)
                        (l, r)

    (* Core *)

    [<RequireQualifiedAccess>]
    module Core =

        let private key =
            Key.add "core" key

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module private Configuration =

            (* Types *)

            type Core =
                { Server: Server
                  Client: Client }

                static member server_ =
                    (fun x -> x.Server), (fun s x -> { x with Server = s })

                static member client_ =
                    (fun x -> x.Client), (fun c x -> { x with Client = c })

                static member empty =
                    { Server = Server.empty
                      Client = Client.empty }

             and Server =
                { Decisions: ServerDecisions
                  Terminals: ServerTerminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Server.Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Server.Terminals = t })

                static member empty =
                    { Decisions = ServerDecisions.empty
                      Terminals = ServerTerminals.empty }

             and ServerDecisions =
                { ServiceAvailable: Value<bool> option
                  HttpVersionSupported: Value<bool> option }

                static member serviceAvailable_ =
                    (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

                static member httpVersionSupported_ =
                    (fun x -> x.HttpVersionSupported), (fun h x -> { x with HttpVersionSupported = h })

                static member empty =
                    { ServiceAvailable = None
                      HttpVersionSupported = None }

             and ServerTerminals =
                { ServiceUnavailable: Freya<unit> option
                  HttpVersionNotSupported: Freya<unit> option
                  NotImplemented: Freya<unit> option }

                static member serviceUnavailable_ =
                    (fun x -> x.ServiceUnavailable), (fun s x -> { x with ServiceUnavailable = s })

                static member httpVersionNotSupported_ =
                    (fun x -> x.HttpVersionNotSupported), (fun h x -> { x with HttpVersionNotSupported = h })

                static member notImplemented_ =
                    (fun x -> x.NotImplemented), (fun n x -> { x with NotImplemented = n })

                static member empty =
                    { ServiceUnavailable = None
                      HttpVersionNotSupported = None
                      NotImplemented = None }

             and Client =
                { Access: Access
                  Request: Request
                  Acceptable: Acceptable }

                static member access_ =
                    (fun x -> x.Access), (fun a x -> { x with Access = a })

                static member request_ =
                    (fun x -> x.Request), (fun r x -> { x with Request = r })

                static member acceptable_ =
                    (fun x -> x.Acceptable), (fun a x -> { x with Acceptable = a })

                static member empty =
                    { Access = Access.empty
                      Request = Request.empty
                      Acceptable = Acceptable.empty }

             and Access =
                { Decisions: AccessDecisions
                  Terminals: AccessTerminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Access.Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Access.Terminals = t })

                static member empty =
                    { Decisions = AccessDecisions.empty
                      Terminals = AccessTerminals.empty }

             and AccessDecisions =
                { Authorized: Value<bool> option
                  Allowed: Value<bool> option }

                static member authorized_ =
                    (fun x -> x.Authorized), (fun a x -> { x with Authorized = a })

                static member allowed_ =
                    (fun x -> x.Allowed), (fun a x -> { x with Allowed = a })

                static member empty =
                    { Authorized = None
                      Allowed = None }

             and AccessTerminals =
                { Unauthorized: Freya<unit> option
                  Forbidden: Freya<unit> option }

                static member unauthorized_ =
                    (fun x -> x.Unauthorized), (fun u x -> { x with Unauthorized = u })

                static member forbidden_ =
                    (fun x -> x.Forbidden), (fun u x -> { x with Forbidden = u })

                static member empty =
                    { Unauthorized = None
                      Forbidden = None }

             and Request =
                { Decisions: RequestDecisions
                  Terminals: RequestTerminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Request.Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Request.Terminals = t })

                static member empty =
                    { Decisions = RequestDecisions.empty
                      Terminals = RequestTerminals.empty }

             and RequestDecisions =
                { ExpectationMet: Value<bool> option
                  MethodAllowed: Value<bool> option
                  UriTooLong: Value<bool> option
                  BadRequest: Value<bool> option }

                static member expectationMet_ =
                    (fun x -> x.ExpectationMet), (fun e x -> { x with ExpectationMet = e })

                static member methodAllowed_ =
                    (fun x -> x.MethodAllowed), (fun m x -> { x with MethodAllowed = m })

                static member uriTooLong_ =
                    (fun x -> x.UriTooLong), (fun u x -> { x with RequestDecisions.UriTooLong = u })

                static member badRequest_ =
                    (fun x -> x.BadRequest), (fun b x -> { x with RequestDecisions.BadRequest = b })

                static member empty =
                    { ExpectationMet = None
                      MethodAllowed = None
                      UriTooLong = None
                      BadRequest = None }

             and RequestTerminals =
                { ExpectationFailed: Freya<unit> option
                  MethodNotAllowed: Freya<unit> option
                  UriTooLong: Freya<unit> option
                  BadRequest: Freya<unit> option }

                static member expectationFailed_ =
                    (fun x -> x.ExpectationFailed), (fun e x -> { x with ExpectationFailed = e })

                static member methodNotAllowed_ =
                    (fun x -> x.MethodNotAllowed), (fun e x -> { x with MethodNotAllowed = e })

                static member uriTooLong_ =
                    (fun x -> x.UriTooLong), (fun u x -> { x with RequestTerminals.UriTooLong = u })

                static member badRequest_ =
                    (fun x -> x.BadRequest), (fun b x -> { x with RequestTerminals.BadRequest = b })

                static member empty =
                    { ExpectationFailed = None
                      MethodNotAllowed = None
                      UriTooLong = None
                      BadRequest = None }

             and Acceptable =
                { Terminals: AcceptableTerminals }

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Acceptable.Terminals = t })

                static member empty =
                    { Terminals = AcceptableTerminals.empty }

             and AcceptableTerminals =
                { NotAcceptable: Freya<unit> option }

                static member notAcceptable_ =
                    (fun x -> x.NotAcceptable), (fun n x -> { x with NotAcceptable = n })

                static member empty =
                    { NotAcceptable = None }

            (* Optics *)

            let core_ =
                Configuration.element_ Core.empty "core"

        (* Core.Server *)

        [<RequireQualifiedAccess>]
        module Server =

            let private key =
                Key.add "server" key

            let private server_ =
                    Configuration.core_
                >-> Configuration.Core.server_

            (* Terminals *)

            [<RequireQualifiedAccess>]
            module Terminals =

                let private terminals_ =
                        server_
                    >-> Configuration.Server.terminals_

                let serviceUnavailable_ =
                        terminals_
                    >-> Configuration.ServerTerminals.serviceUnavailable_

                let httpVersionNotSupported_ =
                        terminals_
                    >-> Configuration.ServerTerminals.httpVersionNotSupported_

                let notImplemented_ =
                        terminals_
                    >-> Configuration.ServerTerminals.notImplemented_

                let internal serviceUnavailable =
                    terminal (key, "service-unavailable-terminal")
                        (Terminal.fromConfigurationWithOperation serviceUnavailable_ Operations.serviceUnavailable)

                let internal httpVersionNotSupported =
                    terminal (key, "http-version-not-supported-terminal")
                        (Terminal.fromConfigurationWithOperation httpVersionNotSupported_ Operations.httpVersionNotSupported)

                let internal notImplemented =
                    terminal (key, "not-implemented-terminal")
                        (Terminal.fromConfigurationWithOperation notImplemented_ Operations.notImplemented)

            (* Decisions *)

            [<RequireQualifiedAccess>]
            module Decisions =

                let private decisions_ =
                        server_
                    >-> Configuration.Server.decisions_

                let serviceAvailable_ =
                        decisions_
                    >-> Configuration.ServerDecisions.serviceAvailable_

                let httpVersionSupported_ =
                        decisions_
                    >-> Configuration.ServerDecisions.httpVersionSupported_

                let rec internal serviceAvailable s =
                    decision (key, "service-available-decision")
                        (Decision.fromConfigurationOrTrue serviceAvailable_)
                        (Terminals.serviceUnavailable, httpVersionSupported s)

                and internal httpVersionSupported s =
                    decision (key, "http-version-supported-decision")
                        (Decision.fromConfigurationOrTrue httpVersionSupported_)
                        (Terminals.httpVersionNotSupported, methodImplemented s)

                // TODO: Not Implemented Logic

                and internal methodImplemented s =
                    decision (key, "method-implemented-decision")
                        (fun _ -> Static true)
                        (Terminals.notImplemented, s)

            (* Root *)

            let internal root =
                Decisions.serviceAvailable

        (* Core.Client *)

        [<RequireQualifiedAccess>]
        module Client =

            let private key =
                Key.add "client" key

            let private client_ =
                    Configuration.core_
                >-> Configuration.Core.client_

            (* Core.Client.Access *)

            [<RequireQualifiedAccess>]
            module Access =

                let private key =
                    Key.add "access" key

                let private access_ =
                        client_
                    >-> Configuration.Client.access_

                [<RequireQualifiedAccess>]
                module Terminals =

                    let private terminals_ =
                            access_
                        >-> Configuration.Access.terminals_

                    let unauthorized_ =
                            terminals_
                        >-> Configuration.AccessTerminals.unauthorized_

                    let forbidden_ =
                            terminals_
                        >-> Configuration.AccessTerminals.forbidden_

                    let internal unauthorized =
                        terminal (key, "unauthorized-terminal")
                            (Terminal.fromConfigurationWithOperation unauthorized_ Operations.unauthorized)

                    let internal forbidden =
                        terminal (key, "forbidden-terminal")
                            (Terminal.fromConfigurationWithOperation forbidden_ Operations.forbidden)

                [<RequireQualifiedAccess>]
                module Decisions =

                    let private decisions_ =
                            access_
                        >-> Configuration.Access.decisions_

                    let authorized_ =
                            decisions_
                        >-> Configuration.AccessDecisions.authorized_

                    let allowed_ =
                            decisions_
                        >-> Configuration.AccessDecisions.allowed_

                    let rec internal authorized s =
                        decision (key, "authorized-decision")
                            (Decision.fromConfigurationOrTrue authorized_)
                            (Terminals.unauthorized, allowed s)

                    and internal allowed s =
                        decision (key, "allowed-decision")
                            (Decision.fromConfigurationOrTrue allowed_)
                            (Terminals.forbidden, s)

                (* Root *)

                let internal root =
                    Decisions.authorized

            (* Core.Client.Request *)

            [<RequireQualifiedAccess>]
            module Request =

                let private key =
                    Key.add "request" key

                let private request_ =
                        client_
                    >-> Configuration.Client.request_

                (* Terminals *)

                [<RequireQualifiedAccess>]
                module Terminals =

                    let private terminals_ =
                            request_
                        >-> Configuration.Request.terminals_

                    let expectationFailed_ =
                            terminals_
                        >-> Configuration.RequestTerminals.expectationFailed_

                    let methodNotAllowed_ =
                            terminals_
                        >-> Configuration.RequestTerminals.methodNotAllowed_

                    let uriTooLong_ =
                            terminals_
                        >-> Configuration.RequestTerminals.uriTooLong_

                    let badRequest_ =
                            terminals_
                        >-> Configuration.RequestTerminals.badRequest_

                    let internal expectationFailed =
                        terminal (key, "expectation-failed-terminal")
                            (Terminal.fromConfigurationWithOperation expectationFailed_ Operations.expectationFailed)

                    let internal methodNotAllowed =
                        terminal (key, "method-not-allowed-terminal")
                            (Terminal.fromConfigurationWithOperation methodNotAllowed_ Operations.methodNotAllowed)

                    let internal uriTooLong =
                        terminal (key, "uri-too-long-terminal")
                            (Terminal.fromConfigurationWithOperation uriTooLong_ Operations.uriTooLong)

                    let internal badRequest =
                        terminal (key, "bad-request-terminal")
                            (Terminal.fromConfigurationWithOperation badRequest_ Operations.badRequest)

                (* Decisions *)

                [<RequireQualifiedAccess>]
                module Decisions =

                    let private decisions_ =
                            request_
                        >-> Configuration.Request.decisions_

                    let expectationMet_ =
                            decisions_
                        >-> Configuration.RequestDecisions.expectationMet_

                    let methodAllowed_ =
                            decisions_
                        >-> Configuration.RequestDecisions.methodAllowed_

                    let uriTooLong_ =
                            decisions_
                        >-> Configuration.RequestDecisions.uriTooLong_

                    let badRequest_ =
                            decisions_
                        >-> Configuration.RequestDecisions.badRequest_

                    // TODO: Expectation Met logic

                    let rec internal expectationMet s =
                        decision (key, "expectation-met-decision")
                            (Decision.fromConfigurationOrTrue expectationMet_)
                            (Terminals.expectationFailed, methodAllowed s)

                    and internal methodAllowed s =
                        decision (key, "method-allowed-decision")
                            (Decision.fromConfigurationOrTrue methodAllowed_)
                            (Terminals.methodNotAllowed, uriTooLong s)

                    and internal uriTooLong s =
                        decision (key, "uri-too-long-decision")
                            (Decision.fromConfigurationOrTrue uriTooLong_)
                            (Terminals.uriTooLong, badRequest s)

                    and internal badRequest s =
                        decision (key, "bad-request-decision")
                            (Decision.fromConfigurationOrTrue badRequest_)
                            (Terminals.badRequest, s)

                (* Root *)

                let internal root =
                    Decisions.expectationMet

            (* Core.Client.Acceptable *)

            [<RequireQualifiedAccess>]
            module Acceptable =

                let private key =
                    Key.add "acceptable" key

                let private acceptable_ =
                        client_
                    >-> Configuration.Client.acceptable_

                (* Terminals *)

                [<RequireQualifiedAccess>]
                module Terminals =

                    let private terminals_ =
                            acceptable_
                        >-> Configuration.Acceptable.terminals_

                    let notAcceptable_ =
                            terminals_
                        >-> Configuration.AcceptableTerminals.notAcceptable_

                    let internal notAcceptable =
                        terminal (key, "not-acceptable-terminal")
                            (Terminal.fromConfigurationWithOperation notAcceptable_ Operations.notAcceptable)

                (* Decisions *)

                module Decisions =

                    let private accept_ =
                            Request.Headers.accept_

                    let private acceptCharset_ =
                            Request.Headers.acceptCharset_

                    let private acceptEncoding_ =
                            Request.Headers.acceptEncoding_

                    let private acceptLanguage_ =
                            Request.Headers.acceptLanguage_

                    let private charsetsSupported_ =
                            Common.Properties.charsetsSupported_

                    let private contentCodingsSupported_ =
                            Common.Properties.contentCodingsSupported_

                    let private mediaTypesSupported_ =
                            Common.Properties.mediaTypesSupported_

                    let private languagesSupported_ =
                            Common.Properties.languagesSupported_

                    let rec internal hasAccept s =
                        decision (key, "has-accept-decision")
                            (fun _ -> Dynamic (Option.isSome <!> !. accept_))
                            (hasAcceptLanguage s, acceptMatches s)

                    and internal acceptMatches s =
                        decision (key, "accept-matches-decision")
                            (function | TryGet mediaTypesSupported_ (Dynamic m) -> Dynamic (MediaType.negotiable <!> m <*> !. accept_)
                                      | TryGet mediaTypesSupported_ (Static m) -> Dynamic (MediaType.negotiable m <!> !. accept_)
                                      | _ -> Static true)
                            (Terminals.notAcceptable, hasAcceptLanguage s)

                    and internal hasAcceptLanguage s =
                        decision (key, "has-accept-language-decision")
                            (fun _ -> Dynamic (Option.isSome <!> !. acceptLanguage_))
                            (hasAcceptCharset s, acceptLanguageMatches s)

                    and internal acceptLanguageMatches s =
                        decision (key, "accept-language-matches-decision")
                            (function | TryGet languagesSupported_ (Dynamic l) -> Dynamic (Language.negotiable <!> l <*> !. acceptLanguage_)
                                      | TryGet languagesSupported_ (Static l) -> Dynamic (Language.negotiable l <!> !. acceptLanguage_)
                                      | _ -> Static true)
                            (Terminals.notAcceptable, hasAcceptCharset s)

                    and internal hasAcceptCharset s =
                        decision (key, "has-accept-charset-decision")
                            (fun _ -> Dynamic (Option.isSome <!> !. acceptCharset_))
                            (hasAcceptEncoding s, acceptCharsetMatches s)

                    and internal acceptCharsetMatches s =
                        decision (key, "accept-charset-matches-decision")
                            (function | TryGet charsetsSupported_ (Dynamic c) -> Dynamic (Charset.negotiable <!> c <*> !. acceptCharset_)
                                      | TryGet charsetsSupported_ (Static c) -> Dynamic (Charset.negotiable c <!> !. acceptCharset_)
                                      | _ -> Static true)
                            (Terminals.notAcceptable, hasAcceptEncoding s)

                    and internal hasAcceptEncoding s =
                        decision (key, "has-accept-encoding-decision")
                            (fun _ -> Dynamic (Option.isSome <!> !. acceptEncoding_))
                            (s, acceptEncodingMatches s)

                    and internal acceptEncodingMatches s =
                        decision (key, "accept-encoding-matches-decision")
                            (function | TryGet contentCodingsSupported_ (Dynamic c) -> Dynamic (ContentCoding.negotiable <!> c <*> !. acceptEncoding_)
                                      | TryGet contentCodingsSupported_ (Static c) -> Dynamic (ContentCoding.negotiable c <!> !. acceptEncoding_)
                                      | _ -> Static true)
                            (Terminals.notAcceptable, s)

                (* Root *)

                let internal root =
                    Decisions.hasAccept

        (* Component *)

        let private specification =
                Server.root
             >> Client.Access.root
             >> Client.Request.root
             >> Client.Acceptable.root

        let internal export =
            { Metadata =
                { Name = "http-core"
                  Description = None }
              Requirements =
                { Required = Set.empty
                  Preconditions = List.empty }
              Operations =
                [ Prepend (fun _ -> specification Common.Terminals.ok) ] }

    (* Model *)

    let model =
        Model.create (set [ Core.export ])

(* Machine

   Mechanics and implementation of the Hephaestus Machine constructed from the
   Model defined previously. The module wraps the base functions for creating
   Prototypes and Machines from Models present within Hephaestus. *)

[<RequireQualifiedAccess>]
module internal Machine =

    (* Evaluation *)

    [<RequireQualifiedAccess>]
    module Evaluation =

        let evaluate machine =
            Machine.execute machine

    (* Reification *)

    [<RequireQualifiedAccess>]
    module Reification =

        let private prototype, prototypeLog =
            Prototype.createLogged Model.model

        let reify configuration =
            let machine, machineLog = Machine.createLogged prototype configuration

            Evaluation.evaluate machine

(* Inference

   Type inference functions for conversion of various forms to a single form,
   for example the conversion of functions and literal values to be
   automatically inferred at compile time to be Literal or Function types of
   Decision.

   This gives a more flexible API where it is used, although at the cost of
   greater documentation/lesser discoverability initially. *)

[<RequireQualifiedAccess>]
module Infer =

    (* Value<'a> *)

    module Value =

        type Defaults =
            | Defaults

            static member Value (x: Freya<'a>) =
                Dynamic x

            static member Value (x: 'a) =
                Static x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Value: ^a -> Value<_>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline value v =
        Value.infer v

    (* MediaType list *)

    module MediaTypes =

        type Defaults =
            | Defaults

            static member MediaTypes (x: Freya<MediaType list>) =
                Dynamic x

            static member MediaTypes (x: MediaType list) =
                Static x

            static member MediaTypes (x: MediaType) = 
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member MediaTypes: ^a -> Value<MediaType list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline mediaTypes v =
        MediaTypes.infer v

    (* Method list *)

    module Methods =

        type Defaults =
            | Defaults

            static member Methods (x: Freya<Method list>) =
                Dynamic x

            static member Methods (x: Method list) =
                Static x

            static member Methods (x: Method) =
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Methods: ^a -> Value<Method list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline methods v =
        Methods.infer v

(* Types

   The base type of an HTTP Machine, representing the user facing type defined
   through the use of the following computation expression.

   The function itself is defined as a single case discriminated union so that
   it can have static members, allowing it to take part in the static inference
   approaches of the basic Freya function, and Pipelines (allowing the pseudo
   typeclass approach which Freya uses in various places for concise APIs). *)

type HttpMachine =
    | HttpMachine of (Configuration -> unit * Configuration)

    (* Common *)

    static member Init _ : HttpMachine =
        HttpMachine (fun c ->
            (), c)

    static member Bind (m: HttpMachine, f: unit -> HttpMachine) : HttpMachine =
        HttpMachine (fun c ->
            let (HttpMachine m) = m
            let (HttpMachine f) = f ()

            (), snd (f (snd (m c))))

    (* Custom *)

    static member Map (m: HttpMachine, f: Configuration -> Configuration) : HttpMachine =
        HttpMachine (fun c ->
            let (HttpMachine m) = m

            (), f (snd (m c)))

    (* Typeclasses *)

    static member Freya (HttpMachine machine) : Freya<_> =
            Machine.Reification.reify (snd (machine Configuration.empty))

    static member Pipeline (HttpMachine machine) : Pipeline =
            HttpMachine.Freya (HttpMachine machine)
         *> Pipeline.next

(* Builder

   Computation expression builder for configuring the HTTP Machine, providing a
   simple type-safe syntax and static inference based overloads of single
   functions.
   
   The builder uses the basic configuration builder defined in Freya.Core, which
   only requires the supply of init and bind functions of the appropriate types
   to implement a type suitable for declarative configuration. *)

type HttpMachineBuilder () =

    inherit Configuration.Builder<HttpMachine>
        { Init = HttpMachine.Init
          Bind = HttpMachine.Bind }

(* Syntax

   Custom syntax expressions for the HTTP Machine computation expression
   builder, giving strongly typed syntax for the configuration elements that
   can be set as part of an HTTP Machine. *)

type HttpMachineBuilder with

    (* Common Properties *)

    [<CustomOperation ("mediaTypesSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.MediaTypesSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Common.Properties.mediaTypesSupported_ (Some (Infer.mediaTypes a)))

    (* Common Terminals *)

    [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleOk (m, a) =
        HttpMachine.Map (m, Optic.set Model.Common.Terminals.ok_ (Some a))

    (* Core.Server Decisions *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Decisions.serviceAvailable_ (Some (Infer.value a)))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Decisions.httpVersionSupported_ (Some (Infer.value a)))

    (* Core.Server Terminals *)

    [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleServiceUnavailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Terminals.serviceUnavailable_ (Some a))

    (* Core.Client.Access Decisions *)

    [<CustomOperation ("authorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Authorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Client.Access.Decisions.authorized_ (Some (Infer.value a)))

    [<CustomOperation ("allowed", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Allowed (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Client.Access.Decisions.allowed_ (Some (Infer.value a)))

    (* Core.Client.Access Terminals *)

    [<CustomOperation ("handleUnauthorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleUnauthorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Client.Access.Terminals.unauthorized_ (Some a))

    [<CustomOperation ("handleForbidden", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleForbidden (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Client.Access.Terminals.forbidden_ (Some a))

(* Expressions

   Computation expressions, instances of the HTTP Machine computation
   expression builder. The fully named instance, freyaHttpMachine is aliased to
   freyaMachine to provide the possibility of more concise code when only one
   kind of machine is in scope.

   This naming also matches the original single form approach to machines, and
   provides backwards compatibility. *)

let freyaHttpMachine =
    HttpMachineBuilder ()

let freyaMachine =
    freyaHttpMachine
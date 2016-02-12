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
module Negotiation =

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

    let preconditionFailed =
            status 412
         *> phrase "Precondition Failed"
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

    (* Prelude

       Model specific prelude providing functions for working with the varying
       elements of models, such as shorthand for working with keys, specifications,
       etc. *)

    [<AutoOpen>]
    module internal Prelude =

        (* Keys

           Functions for working with Hephaestus Keys, making defining and using
           keys slightly more pleasant. The default empty key is included here
           for consistency at the various levels. *)

        [<RequireQualifiedAccess>]
        module Key =

            let add x =
                Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) x)

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
            Specification.Decision.create (Key.add [ name ] key) (configurator >> Decision.map) (t, f)

        let internal terminal (key, name) configurator =
            Specification.Terminal.create (Key.add [ name ] key) configurator

    (* Key

       The root key for the "namespace" like functionality implied by the key
       mechanism used in Hephaestus. The machine name is used as the common
       key present in all specifications in this instance. *)

    let private key =
        Key.add [ "http" ] Key.empty

    (* Properties

       Properties of the resource which the machine represents, defined as a
       distinct set of types as these may be used/shared by any of the many
       elements which might be defined as part of an HTTP machine. *)

    [<AutoOpen>]
    module Properties =

        (* Types *)

        type private Properties =
            { Representation: Representation
              Resource: Resource }

            static member representation_ =
                (fun x -> x.Representation), (fun r x -> { x with Representation = r })

            static member resource_ =
                (fun x -> x.Resource), (fun r x -> { x with Resource = r })

            static member empty =
                { Representation = Representation.empty
                  Resource = Resource.empty }

         and private Representation =
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

         and private Resource =
            { ETags: Value<ETag list> option
              LastModified: Value<DateTime> option }

            static member eTags_ =
                (fun x -> x.ETags), (fun e x -> { x with ETags = e })

            static member lastModified_ =
                (fun x -> x.LastModified), (fun l x -> { x with LastModified = l })

            static member empty =
                { ETags = None
                  LastModified = None }

        (* Optics *)

        let private properties_ =
                Configuration.element_ Properties.empty "properties"

        let private representation_ =
                properties_
            >-> Properties.representation_

        let private resource_ =
                properties_
            >-> Properties.resource_

        (* Representation *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Representation =

            let charsetsSupported_ =
                    representation_
                >-> Representation.charsetsSupported_

            let contentCodingsSupported_ =
                    representation_
                >-> Representation.contentCodingsSupported_

            let languagesSupported_ =
                    representation_
                >-> Representation.languagesSupported_

            let mediaTypesSupported_ =
                    representation_
                >-> Representation.mediaTypesSupported_

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Resource =

            let eTags_ =
                    resource_
                >-> Resource.eTags_

            let lastModified_ =
                    resource_
                >-> Resource.lastModified_

    (* Elements

       Elements (in some cases parameterizable) which may be combined to make
       up specific components which form an HTTP machine. The elements each
       form a specific piece of functionality and are name accordingly. *)

    [<AutoOpen>]
    module Elements =

        (* Server *)

        [<RequireQualifiedAccess>]
        module Server =

            (* Key *)

            let private key p =
                Key.add [ p; "server" ] key

            (* Types *)

            type private Server =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Server.Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Server.Terminals = t })

                static member empty =
                    { Decisions = Decisions.empty
                      Terminals = Terminals.empty }

             and private Decisions =
                { ServiceAvailable: Value<bool> option
                  HttpVersionSupported: Value<bool> option }

                static member serviceAvailable_ =
                    (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

                static member httpVersionSupported_ =
                    (fun x -> x.HttpVersionSupported), (fun h x -> { x with HttpVersionSupported = h })

                static member empty =
                    { ServiceAvailable = None
                      HttpVersionSupported = None }

             and private Terminals =
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

            (* Optics *)

            let private server_ =
                    Configuration.element_ Server.empty "server"

            (* Terminals *)

            let private terminals_ =
                    server_
                >-> Server.terminals_

            let serviceUnavailableTerminal_ =
                    terminals_
                >-> Terminals.serviceUnavailable_

            let httpVersionNotSupportedTerminal_ =
                    terminals_
                >-> Terminals.httpVersionNotSupported_

            let notImplementedTerminal_ =
                    terminals_
                >-> Terminals.notImplemented_

            let private serviceUnavailableTerminal p =
                terminal (key p, "service-unavailable-terminal")
                    (Terminal.fromConfigurationWithOperation serviceUnavailableTerminal_ Operations.serviceUnavailable)

            let private httpVersionNotSupportedTerminal p =
                terminal (key p, "http-version-not-supported-terminal")
                    (Terminal.fromConfigurationWithOperation httpVersionNotSupportedTerminal_ Operations.httpVersionNotSupported)

            let private notImplementedTerminal p =
                terminal (key p, "not-implemented-terminal")
                    (Terminal.fromConfigurationWithOperation notImplementedTerminal_ Operations.notImplemented)

            (* Decisions *)

            let private decisions_ =
                    server_
                >-> Server.decisions_

            let serviceAvailableDecision_ =
                    decisions_
                >-> Decisions.serviceAvailable_

            let httpVersionSupportedDecision_ =
                    decisions_
                >-> Decisions.httpVersionSupported_

            let rec private serviceAvailableDecision p s =
                decision (key p, "service-available-decision")
                    (Decision.fromConfigurationOrTrue serviceAvailableDecision_)
                    (serviceUnavailableTerminal p, httpVersionSupportedDecision p s)

            and private httpVersionSupportedDecision p s =
                decision (key p, "http-version-supported-decision")
                    (Decision.fromConfigurationOrTrue httpVersionSupportedDecision_)
                    (httpVersionNotSupportedTerminal p, methodImplementedDecision p s)

            // TODO: Not Implemented Logic

            and private methodImplementedDecision p s =
                decision (key p, "method-implemented-decision")
                    (fun _ -> Static true)
                    (notImplementedTerminal p, s)

            (* Element *)

            let element =
                serviceAvailableDecision

        (* Client *)

        [<RequireQualifiedAccess>]
        module Client =

            (* Key *)

            let private key p =
                Key.add [ p; "client" ] key

            (* Access *)

            [<RequireQualifiedAccess>]
            module Access =

                (* Key *)

                let private key p =
                    Key.add [ "access" ] (key p)

                (* Types *)

                type private Access =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Access.Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Access.Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { Authorized: Value<bool> option
                      Allowed: Value<bool> option }

                    static member authorized_ =
                        (fun x -> x.Authorized), (fun a x -> { x with Authorized = a })

                    static member allowed_ =
                        (fun x -> x.Allowed), (fun a x -> { x with Allowed = a })

                    static member empty =
                        { Authorized = None
                          Allowed = None }

                 and private Terminals =
                    { Unauthorized: Freya<unit> option
                      Forbidden: Freya<unit> option }

                    static member unauthorized_ =
                        (fun x -> x.Unauthorized), (fun u x -> { x with Unauthorized = u })

                    static member forbidden_ =
                        (fun x -> x.Forbidden), (fun u x -> { x with Forbidden = u })

                    static member empty =
                        { Unauthorized = None
                          Forbidden = None }

                (* Optics *)

                let private access_ =
                    Configuration.element_ Access.empty "client.access"

                (* Terminals *)

                let private terminals_ =
                        access_
                    >-> Access.terminals_

                let unauthorizedTerminal_ =
                        terminals_
                    >-> Terminals.unauthorized_

                let forbiddenTerminal_ =
                        terminals_
                    >-> Terminals.forbidden_

                let private unauthorizedTerminal p =
                    terminal (key p, "unauthorized-terminal")
                        (Terminal.fromConfigurationWithOperation unauthorizedTerminal_ Operations.unauthorized)

                let private forbiddenTerminal p =
                    terminal (key p, "forbidden-terminal")
                        (Terminal.fromConfigurationWithOperation forbiddenTerminal_ Operations.forbidden)

                (* Decisions *)

                let private decisions_ =
                        access_
                    >-> Access.decisions_

                let authorizedDecision_ =
                        decisions_
                    >-> Decisions.authorized_

                let allowedDecision_ =
                        decisions_
                    >-> Decisions.allowed_

                let rec private authorizedDecision p s =
                    decision (key p, "authorized-decision")
                        (Decision.fromConfigurationOrTrue authorizedDecision_)
                        (unauthorizedTerminal p, allowedDecision p s)

                and private allowedDecision p s =
                    decision (key p, "allowed-decision")
                        (Decision.fromConfigurationOrTrue allowedDecision_)
                        (forbiddenTerminal p, s)

                (* Element *)

                let element =
                    authorizedDecision

            (* Request *)

            [<RequireQualifiedAccess>]
            module Request =

                (* Key *)

                let private key p =
                    Key.add [ "request" ] (key p)

                (* Types *)

                type private Request =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Request.Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Request.Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { ExpectationMet: Value<bool> option
                      MethodAllowed: Value<bool> option
                      UriTooLong: Value<bool> option
                      BadRequest: Value<bool> option }

                    static member expectationMet_ =
                        (fun x -> x.ExpectationMet), (fun e x -> { x with ExpectationMet = e })

                    static member methodAllowed_ =
                        (fun x -> x.MethodAllowed), (fun m x -> { x with MethodAllowed = m })

                    static member uriTooLong_ =
                        (fun x -> x.UriTooLong), (fun u x -> { x with Decisions.UriTooLong = u })

                    static member badRequest_ =
                        (fun x -> x.BadRequest), (fun b x -> { x with Decisions.BadRequest = b })

                    static member empty =
                        { ExpectationMet = None
                          MethodAllowed = None
                          UriTooLong = None
                          BadRequest = None }

                 and private Terminals =
                    { ExpectationFailed: Freya<unit> option
                      MethodNotAllowed: Freya<unit> option
                      UriTooLong: Freya<unit> option
                      BadRequest: Freya<unit> option }

                    static member expectationFailed_ =
                        (fun x -> x.ExpectationFailed), (fun e x -> { x with ExpectationFailed = e })

                    static member methodNotAllowed_ =
                        (fun x -> x.MethodNotAllowed), (fun e x -> { x with MethodNotAllowed = e })

                    static member uriTooLong_ =
                        (fun x -> x.UriTooLong), (fun u x -> { x with Terminals.UriTooLong = u })

                    static member badRequest_ =
                        (fun x -> x.BadRequest), (fun b x -> { x with Terminals.BadRequest = b })

                    static member empty =
                        { ExpectationFailed = None
                          MethodNotAllowed = None
                          UriTooLong = None
                          BadRequest = None }

                (* Optics *)

                let private request_ =
                    Configuration.element_ Request.empty "client.request"

                (* Terminals *)

                let private terminals_ =
                        request_
                    >-> Request.terminals_

                let expectationFailedTerminal_ =
                        terminals_
                    >-> Terminals.expectationFailed_

                let methodNotAllowedTerminal_ =
                        terminals_
                    >-> Terminals.methodNotAllowed_

                let uriTooLongTerminal_ =
                        terminals_
                    >-> Terminals.uriTooLong_

                let badRequestTerminal_ =
                        terminals_
                    >-> Terminals.badRequest_

                let private expectationFailedTerminal p =
                    terminal (key p, "expectation-failed-terminal")
                        (Terminal.fromConfigurationWithOperation expectationFailedTerminal_ Operations.expectationFailed)

                let private methodNotAllowedTerminal p =
                    terminal (key p, "method-not-allowed-terminal")
                        (Terminal.fromConfigurationWithOperation methodNotAllowedTerminal_ Operations.methodNotAllowed)

                let private uriTooLongTerminal p =
                    terminal (key p, "uri-too-long-terminal")
                        (Terminal.fromConfigurationWithOperation uriTooLongTerminal_ Operations.uriTooLong)

                let private badRequestTerminal p =
                    terminal (key p, "bad-request-terminal")
                        (Terminal.fromConfigurationWithOperation badRequestTerminal_ Operations.badRequest)

                (* Decisions *)

                let private decisions_ =
                        request_
                    >-> Request.decisions_

                let expectationMetDecision_ =
                        decisions_
                    >-> Decisions.expectationMet_

                let methodAllowedDecision_ =
                        decisions_
                    >-> Decisions.methodAllowed_

                let uriTooLongDecision_ =
                        decisions_
                    >-> Decisions.uriTooLong_

                let badRequestDecision_ =
                        decisions_
                    >-> Decisions.badRequest_

                // TODO: Expectation Met logic

                let rec private expectationMetDecision p s =
                    decision (key p, "expectation-met-decision")
                        (Decision.fromConfigurationOrTrue expectationMetDecision_)
                        (expectationFailedTerminal p, methodAllowedDecision p s)

                and private methodAllowedDecision p s =
                    decision (key p, "method-allowed-decision")
                        (Decision.fromConfigurationOrTrue methodAllowedDecision_)
                        (methodNotAllowedTerminal p, uriTooLongDecision p s)

                and private uriTooLongDecision p s =
                    decision (key p, "uri-too-long-decision")
                        (Decision.fromConfigurationOrFalse uriTooLongDecision_)
                        (badRequestDecision p s, uriTooLongTerminal p)

                and private badRequestDecision p s =
                    decision (key p, "bad-request-decision")
                        (Decision.fromConfigurationOrFalse badRequestDecision_)
                        (s, badRequestTerminal p)

                (* Element *)

                let element =
                    expectationMetDecision

            (* Acceptable *)

            [<RequireQualifiedAccess>]
            module Acceptable =

                (* Key *)

                let private key p =
                    Key.add [ "acceptable" ] (key p)

                (* Types *)

                type private Acceptable =
                    { Terminals: Terminals }

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Acceptable.Terminals = t })

                    static member empty =
                        { Terminals = Terminals.empty }

                 and private Terminals =
                    { NotAcceptable: Freya<unit> option }

                    static member notAcceptable_ =
                        (fun x -> x.NotAcceptable), (fun n x -> { x with NotAcceptable = n })

                    static member empty =
                        { NotAcceptable = None }

                (* Optics *)

                let private acceptable_ =
                    Configuration.element_ Acceptable.empty "client.acceptable"

                (* Terminals *)

                let private terminals_ =
                        acceptable_
                    >-> Acceptable.terminals_

                let notAcceptableTerminal_ =
                        terminals_
                    >-> Terminals.notAcceptable_

                let private notAcceptableTerminal p =
                    terminal (key p, "not-acceptable-terminal")
                        (Terminal.fromConfigurationWithOperation notAcceptableTerminal_ Operations.notAcceptable)

                (* Decisions *)

                let private accept_ =
                        Request.Headers.accept_

                let private acceptCharset_ =
                        Request.Headers.acceptCharset_

                let private acceptEncoding_ =
                        Request.Headers.acceptEncoding_

                let private acceptLanguage_ =
                        Request.Headers.acceptLanguage_

                let private charsetsSupported_ =
                        Representation.charsetsSupported_

                let private contentCodingsSupported_ =
                        Representation.contentCodingsSupported_

                let private mediaTypesSupported_ =
                        Representation.mediaTypesSupported_

                let private languagesSupported_ =
                        Representation.languagesSupported_

                let rec private hasAcceptDecision p s =
                    decision (key p, "has-accept-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. accept_))
                        (hasAcceptLanguageDecision p s, acceptMatchesDecision p s)

                and private acceptMatchesDecision p s =
                    decision (key p, "accept-matches-decision")
                        (function | TryGet mediaTypesSupported_ (Dynamic m) -> Dynamic (MediaType.negotiable <!> m <*> !. accept_)
                                  | TryGet mediaTypesSupported_ (Static m) -> Dynamic (MediaType.negotiable m <!> !. accept_)
                                  | _ -> Static true)
                        (notAcceptableTerminal p, hasAcceptLanguageDecision p s)

                and private hasAcceptLanguageDecision p s =
                    decision (key p, "has-accept-language-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. acceptLanguage_))
                        (hasAcceptCharsetDecision p s, acceptLanguageMatchesDecision p s)

                and private acceptLanguageMatchesDecision p s =
                    decision (key p, "accept-language-matches-decision")
                        (function | TryGet languagesSupported_ (Dynamic l) -> Dynamic (Language.negotiable <!> l <*> !. acceptLanguage_)
                                  | TryGet languagesSupported_ (Static l) -> Dynamic (Language.negotiable l <!> !. acceptLanguage_)
                                  | _ -> Static true)
                        (notAcceptableTerminal p, hasAcceptCharsetDecision p s)

                and private hasAcceptCharsetDecision p s =
                    decision (key p, "has-accept-charset-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. acceptCharset_))
                        (hasAcceptEncodingDecision p s, acceptCharsetMatchesDecision p s)

                and private acceptCharsetMatchesDecision p s =
                    decision (key p, "accept-charset-matches-decision")
                        (function | TryGet charsetsSupported_ (Dynamic c) -> Dynamic (Charset.negotiable <!> c <*> !. acceptCharset_)
                                  | TryGet charsetsSupported_ (Static c) -> Dynamic (Charset.negotiable c <!> !. acceptCharset_)
                                  | _ -> Static true)
                        (notAcceptableTerminal p, hasAcceptEncodingDecision p s)

                and private hasAcceptEncodingDecision p s =
                    decision (key p, "has-accept-encoding-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. acceptEncoding_))
                        (s, acceptEncodingMatchesDecision p s)

                and private acceptEncodingMatchesDecision p s =
                    decision (key p, "accept-encoding-matches-decision")
                        (function | TryGet contentCodingsSupported_ (Dynamic c) -> Dynamic (ContentCoding.negotiable <!> c <*> !. acceptEncoding_)
                                  | TryGet contentCodingsSupported_ (Static c) -> Dynamic (ContentCoding.negotiable c <!> !. acceptEncoding_)
                                  | _ -> Static true)
                        (notAcceptableTerminal p, s)

                (* Element *)

                let element =
                    hasAcceptDecision

        (* Request *)

        [<RequireQualifiedAccess>]
        module Request =

            (* Key *)

            let private key p =
                Key.add [ p; "request" ] key

            (* Method *)

            [<RequireQualifiedAccess>]
            module Method =

                (* Key *)

                let private key p =
                    Key.add [ "method" ] (key p)

                (* Decisions *)

                let private method_ =
                    Request.method_

                let private methodMatchesDecision p m =
                    decision (key p, "method-matches-decision")
                        (fun _ -> Dynamic (flip List.contains m <!> !. method_))

                (* Element *)

                let element =
                    methodMatchesDecision

    (* Components

       The components of an HTTP machine model, formed by composing and in some
       cases parameterizing elements in specific orders to give a useful HTTP
       processing cycle. *)

    [<AutoOpen>]
    module Components =

        (* Core *)

        [<RequireQualifiedAccess>]
        module Core =

            [<Literal>]
            let private Core =
                "core"

            (* Terminals *)

            let private endpointTerminal =
                terminal (key, "endpoint-terminal")
                    (fun _ -> Operations.ok)

            (* Decisions *)

            let private endpointDecision =
                decision (key, "endpoint-decision")
                    (fun _ -> Static true)
                    (Specification.Terminal.empty, endpointTerminal)

            (* Component *)

            let private core =
                Server.element Core (
                    Client.Access.element Core (
                        Client.Request.element Core (
                            Client.Acceptable.element Core endpointDecision)))

            let export =
                { Metadata =
                    { Name = "http.core"
                      Description = None }
                  Requirements =
                    { Required = Set.empty
                      Preconditions = List.empty }
                  Operations =
                    [ Prepend (fun _ -> core) ] }

        (* Get or Head *)

        [<RequireQualifiedAccess>]
        module GetOrHead =

            [<Literal>]
            let private GetOrHead =
                "get-or-head"

            (* Terminals *)

            let private endpointTerminal : Specification<Configuration,unit,State> =
                terminal (key, "endpoint-terminal-2")
                    (fun _ -> Operations.ok)

            let private getOrHead s =
                Request.Method.element GetOrHead [ GET; HEAD ] (s, endpointTerminal)

            let export =
                { Metadata =
                    { Name = "http.get"
                      Description = None }
                  Requirements =
                    { Required = Set.empty
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "endpoint-decision" ], Right, getOrHead) ] }

    (* Model *)

    let model =
        Model.create (
            set [
                Core.export
                GetOrHead.export ])

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
        HttpMachine.Map (m, Optic.set Model.Properties.Representation.mediaTypesSupported_ (Some (Infer.mediaTypes a)))

//    (* Common Terminals *)
//
//    [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
//    member inline __.HandleOk (m, a) =
//        HttpMachine.Map (m, Optic.set Model.Common.Terminals.ok_ (Some a))

    (* Core.Server Decisions *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Server.serviceAvailableDecision_ (Some (Infer.value a)))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Server.httpVersionSupportedDecision_ (Some (Infer.value a)))

    (* Core.Server Terminals *)

    [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleServiceUnavailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Server.serviceUnavailableTerminal_ (Some a))

    (* Core.Client.Access Decisions *)

    [<CustomOperation ("authorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Authorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Client.Access.authorizedDecision_ (Some (Infer.value a)))

    [<CustomOperation ("allowed", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Allowed (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Client.Access.allowedDecision_ (Some (Infer.value a)))

    (* Core.Client.Access Terminals *)

    [<CustomOperation ("handleUnauthorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleUnauthorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Client.Access.unauthorizedTerminal_ (Some a))

    [<CustomOperation ("handleForbidden", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleForbidden (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Client.Access.forbiddenTerminal_ (Some a))

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
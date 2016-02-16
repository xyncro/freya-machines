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

// TODO: Complete syntax
// TODO: Representation in handlers
// TODO: Complete operations
// TODO: Options
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

(* Defaults *)

[<RequireQualifiedAccess>]
module Defaults =

    let methodsAllowed =
        [ GET
          HEAD
          OPTIONS ]

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

    let options =
            status 200
         *> phrase "Options"
         *> date

    let created =
            status 201
         *> phrase "Created"
         *> date

    let accepted =
            status 202
         *> phrase "Accepted"
         *> date

    let noContent =
            status 204
         *> phrase "No Content"
         *> date

    (* 3xx *)

    let multipleChoices =
            status 300
         *> phrase "Multiple Choices"
         *> date

    let movedPermanently =
            status 301
         *> phrase "Moved Permanently"
         *> date

    let found =
            status 302
         *> phrase "Found"
         *> date

    let seeOther =
            status 303
         *> phrase "See Other"
         *> date

    let notModified =
            status 304
         *> phrase "Not Modified"
         *> date

    let temporaryRedirect =
            status 307
         *> phrase "Temporary Redirect"
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

    let notFound =
            status 404
         *> phrase "Not Found"
         *> date

    let methodNotAllowed =
            status 405
         *> phrase "Method Not Allowed"
         *> date

    let notAcceptable =
            status 406
         *> phrase "Not Acceptable"
         *> date

    let conflict =
            status 409
         *> phrase "Conflict"
         *> date

    let gone =
            status 410
         *> phrase "Gone"
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

    let internalServerError =
            status 500
         *> phrase "Internal Server Error"
         *> date

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
            { Request: Request
              Representation: Representation
              Resource: Resource }

            static member request_ =
                (fun x -> x.Request), (fun r x -> { x with Request = r })


            static member representation_ =
                (fun x -> x.Representation), (fun r x -> { x with Representation = r })

            static member resource_ =
                (fun x -> x.Resource), (fun r x -> { x with Resource = r })

            static member empty =
                { Request = Request.empty
                  Representation = Representation.empty
                  Resource = Resource.empty }

         and private Request =
            { MethodsAllowed: Value<Method list> option }

            static member methodsAllowed_ =
                (fun x -> x.MethodsAllowed), (fun m x -> { x with MethodsAllowed = m })

            static member empty =
                { MethodsAllowed = None }

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

        let private request_ =
                properties_
            >-> Properties.request_

        (* Request *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Request =

            let private request_ =
                    properties_
                >-> Properties.request_

            let methodsAllowed_ =
                    request_
                >-> Request.methodsAllowed_

        (* Representation *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Representation =

            let private representation_ =
                    properties_
                >-> Properties.representation_

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

            let private resource_ =
                    properties_
                >-> Properties.resource_

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

        (* Assertion

           Decisions asserting the capability of the server to handle the given
           request, based on the defined availability, HTTP protocols, etc.

           Failures of these assertions/checks of server capability will result
           in 5xx error responses, signalling a server error of some type. *)

        [<RequireQualifiedAccess>]
        module Assertion =

            (* Key *)

            let private key p =
                Key.add [ p; "assertion" ] key

            (* Types *)

            type private Assertion =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

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

            let private assertion_ =
                    Configuration.element_ Assertion.empty "assertion"

            (* Terminals *)

            let private terminals_ =
                    assertion_
                >-> Assertion.terminals_

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
                    assertion_
                >-> Assertion.decisions_

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

            (* Export *)

            let export =
                serviceAvailableDecision

        (* Permission

           Decisions determining the permission of the client to make the
           current request, whether for reasons of authorization or allowance.

           Failures of these checks will result in  401 or 403 responses,
           signalling a client error. *)

        [<RequireQualifiedAccess>]
        module Permission =

            (* Key *)

            let private key p =
                Key.add [ p; "permission" ] key

            (* Types *)

            type private Permission =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

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

            let private permission_ =
                Configuration.element_ Permission.empty "permission"

            (* Terminals *)

            let private terminals_ =
                    permission_
                >-> Permission.terminals_

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
                    permission_
                >-> Permission.decisions_

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

            (* Export *)

            let export =
                authorizedDecision

        (* Validation

           Decisions determing the basic syntactic and semantic validity of the
           request made by the client, such as whether the method requested is
           allowed, whether the URI is of an appropriate length, etc.

           Failures of these checks will result in 4xx responses of the
           appropriate value, signalling a client error of some type. *)

        [<RequireQualifiedAccess>]
        module Validation =

            (* Key *)

            let private key p =
                Key.add [ p; "validation" ] key

            (* Types *)

            type private Validation =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Decisions = Decisions.empty
                      Terminals = Terminals.empty }

             and private Decisions =
                { ExpectationMet: Value<bool> option
                  UriTooLong: Value<bool> option
                  BadRequest: Value<bool> option }

                static member expectationMet_ =
                    (fun x -> x.ExpectationMet), (fun e x -> { x with ExpectationMet = e })

                static member uriTooLong_ =
                    (fun x -> x.UriTooLong), (fun u x -> { x with Decisions.UriTooLong = u })

                static member badRequest_ =
                    (fun x -> x.BadRequest), (fun b x -> { x with Decisions.BadRequest = b })

                static member empty =
                    { ExpectationMet = None
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

            let private validation_ =
                Configuration.element_ Validation.empty "validation"

            (* Terminals *)

            let private terminals_ =
                    validation_
                >-> Validation.terminals_

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
                    validation_
                >-> Validation.decisions_

            let private method_ =
                    Request.method_

            let private methodsAllowed_ =
                    Properties.Request.methodsAllowed_

            let expectationMetDecision_ =
                    decisions_
                >-> Decisions.expectationMet_

            let uriTooLongDecision_ =
                    decisions_
                >-> Decisions.uriTooLong_

            let badRequestDecision_ =
                    decisions_
                >-> Decisions.badRequest_

            // TODO: Logic

            let rec private expectationMetDecision p s =
                decision (key p, "expectation-met-decision")
                    (Decision.fromConfigurationOrTrue expectationMetDecision_)
                    (expectationFailedTerminal p, methodAllowedDecision p s)

            and private methodAllowedDecision p s =
                decision (key p, "method-allowed-decision")
                    (function | TryGet methodsAllowed_ (Dynamic m) -> Dynamic (flip List.contains <!> m <*> !. method_)
                              | TryGet methodsAllowed_ (Static m) -> Dynamic (flip List.contains m <!> !. method_)
                              | _ -> Dynamic (flip List.contains Defaults.methodsAllowed <!> !. method_))
                    (methodNotAllowedTerminal p, uriTooLongDecision p s)

            and private uriTooLongDecision p s =
                decision (key p, "uri-too-long-decision")
                    (Decision.fromConfigurationOrFalse uriTooLongDecision_)
                    (badRequestDecision p s, uriTooLongTerminal p)

            and private badRequestDecision p s =
                decision (key p, "bad-request-decision")
                    (Decision.fromConfigurationOrFalse badRequestDecision_)
                    (s, badRequestTerminal p)

            (* Export *)

            let export =
                expectationMetDecision

        (* Method

           Decision determining whether the method matches any of a supplied
           list of methods given as part of parameterization of this element.

           The element does not result in a response, only in control flow of
           the machine, and as such must be provided with both left and right
           cases (no terminals are implied).

           NOTE: This decision has a significant optimization, in that it will
           become a Static value of false if the method to be matched is not
           an allowed method for this resource. This aids significantly in
           graph optimization, but does imply that a method validation check
           should be put of the workflow (i.e. that it should correctly be
           preceded by a Validation element). *)

        [<RequireQualifiedAccess>]
        module Method =

            [<RequireQualifiedAccess>]
            module private Seq =

                let disjoint l1 l2 =
                    Set.isEmpty (Set.intersect (set l1) (set l2))

                let identical l1 l2 =
                    (set l1) = (set l2)

            (* Key *)

            let private key p =
                Key.add [ p; "method" ] key

            (* Decisions *)

            let private method_ =
                    Request.method_

            let private methodsAllowed_ =
                    Properties.Request.methodsAllowed_

            let private methodMatchesDecision p ms =
                decision (key p, "method-matches-decision")
                    (function | TryGet methodsAllowed_ (Static ms') when Seq.disjoint ms ms' -> Static false
                              | TryGet methodsAllowed_ _ -> Dynamic (flip List.contains ms <!> !. method_)
                              | _ when Seq.disjoint ms Defaults.methodsAllowed -> Static false
                              | _ -> Dynamic (flip List.contains ms <!> !. method_))

            (* Export *)

            let export =
                methodMatchesDecision

        (* Negotiation

           Decisions determining whether an appropriate representation of the
           resource is available given the specifications of acceptability
           provided by the client (negotiating on media type, language, etc.)

           Where the client does specify a requirement for representation which
           is incompatible with the representation defined by the resource, a
           412 response will result. *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            (* Key *)

            let private key p =
                Key.add [ p; "negotiation" ] key

            (* Types *)

            type private Negotiation =
                { Terminals: Terminals }

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Terminals = Terminals.empty }

             and private Terminals =
                { NotAcceptable: Freya<unit> option }

                static member notAcceptable_ =
                    (fun x -> x.NotAcceptable), (fun n x -> { x with NotAcceptable = n })

                static member empty =
                    { NotAcceptable = None }

            (* Optics *)

            let private negotiation_ =
                Configuration.element_ Negotiation.empty "negotiation"

            (* Terminals *)

            let private terminals_ =
                    negotiation_
                >-> Negotiation.terminals_

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

            (* Export *)

            let export =
                hasAcceptDecision

        (* Existence

           Decision determining whether or not the relevant resource exists at
           this point (it may have existed previously, but this is about the
           existence of the resource currently).

           The element does not result in a response, only in control flow of
           the machine, and as such must be provided with both left and right
           cases (no terminals are implied). *)

        [<RequireQualifiedAccess>]
        module Existence =

            let private key p =
                Key.add [ p; "existence" ] key

            (* Types *)

            type private Existence =
                { Decisions: Decisions }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member empty =
                    { Decisions = Decisions.empty }

             and private Decisions =
                { Exists: Value<bool> option }

                static member exists_ =
                    (fun x -> x.Exists), (fun e x -> { x with Exists = e })

                static member empty =
                    { Exists = None }

            (* Optics *)

            let private existence_ =
                Configuration.element_ Existence.empty "existence"

            (* Decisions *)

            let private decisions_ =
                    existence_
                >-> Existence.decisions_

            let existsDecision_ =
                    decisions_
                >-> Decisions.exists_

            let private existsDecision p =
                decision (key p, "exists-decision")
                    (Decision.fromConfigurationOrTrue existsDecision_)

            (* Export *)

            let export =
                existsDecision

        (* Preconditions *)

        [<RequireQualifiedAccess>]
        module Preconditions =

            (* Key *)

            let private key p =
                Key.add [ p; "preconditions" ] key

            (* Types *)

            type private Preconditions =
                { Terminals: Terminals }

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Terminals = Terminals.empty }

             and Terminals =
                { PreconditionFailed: Freya<unit> option }

                static member preconditionFailed_ =
                    (fun x -> x.PreconditionFailed), (fun p x -> { x with PreconditionFailed = p })

                static member empty =
                    { PreconditionFailed = None }

            (* Optics *)

            let private preconditions_ =
                Configuration.element_ Preconditions.empty "preconditions"

            let eTags_ =
                  Properties.Resource.eTags_

            let lastModified_ =
                  Properties.Resource.lastModified_

            (* Terminals *)

            let private terminals_ =
                    preconditions_
                >-> Preconditions.terminals_

            let preconditionFailedTerminal_ =
                    terminals_
                >-> Terminals.preconditionFailed_

            let private preconditionFailedTerminal p =
                terminal (key p, "precondition-failed-terminal")
                    (Terminal.fromConfigurationWithOperation preconditionFailedTerminal_ Operations.preconditionFailed)

            (* Common *)

            [<RequireQualifiedAccess>]
            module Common =

                (* Key *)

                let private key p =
                    Key.add [ "common" ] (key p)

                (* Decisions *)

                let private ifMatch_ =
                        Request.Headers.ifMatch_

                let private ifUnmodifiedSince_ =
                        Request.Headers.ifUnmodifiedSince_

                let rec private hasIfMatchDecision p s =
                    decision (key p, "has-if-match-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. ifMatch_))
                        (hasIfUnmodifiedSinceDecision p s, ifMatchMatchesDecision p s)

                // TODO: Logic

                and private ifMatchMatchesDecision p s =
                    decision (key p, "if-match-matches-decision")
                        (function | TryGet eTags_ (Dynamic _) -> Static true
                                  | _ -> Static true)
                        (preconditionFailedTerminal p, s)

                and private hasIfUnmodifiedSinceDecision p s =
                    decision (key p, "has-if-unmodified-since-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. ifUnmodifiedSince_))
                        (s, ifUnmodifiedSinceMatchesDecision p s)

                // TODO: Logic

                and private ifUnmodifiedSinceMatchesDecision p s =
                    decision (key p, "if-unmodified-since-matches-decision")
                        (function | TryGet lastModified_ (Dynamic _) -> Static true
                                  | _ -> Static true)
                        (preconditionFailedTerminal p, s)

                (* Export *)

                let export =
                    hasIfMatchDecision

            (* Safe *)

            [<RequireQualifiedAccess>]
            module Safe =

                (* Key *)

                let private key p =
                    Key.add [ "safe" ] (key p)

                (* Types *)

                type private Safe =
                    { Terminals: Terminals }

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Safe.Terminals = t })

                    static member empty =
                        { Terminals = Terminals.empty }

                 and Terminals =
                    { NotModified: Freya<unit> option }

                    static member notModified_ =
                        (fun x -> x.NotModified), (fun n x -> { x with NotModified = n })

                    static member empty =
                        { NotModified = None }

                (* Optics *)

                let private safe_ =
                    Configuration.element_ Safe.empty "preconditions.safe"

                (* Terminals *)

                let private terminals_ =
                        safe_
                    >-> Safe.terminals_

                let private notModifiedTerminal_ =
                        terminals_
                    >-> Terminals.notModified_

                let private notModifiedTerminal p =
                    terminal (key p, "not-modified-terminal")
                        (Terminal.fromConfigurationWithOperation notModifiedTerminal_ Operations.notModified)

                (* Decisions *)

                let private ifNoneMatch_ =
                        Request.Headers.ifNoneMatch_

                let private ifModifiedSince_ =
                        Request.Headers.ifModifiedSince_

                let rec private hasIfNoneMatchDecision p s =
                    decision (key p, "has-if-none-match-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. ifNoneMatch_))
                        (hasIfModifiedSinceDecision p s, ifNoneMatchMatches p s)

                // TODO: Logic

                and private ifNoneMatchMatches p s =
                    decision (key p, "if-none-match-matches-decision")
                        (function | _ -> Static true)
                        (notModifiedTerminal p, s)

                and private hasIfModifiedSinceDecision p s =
                    decision (key p, "has-if-modified-since-decision")
                        (fun _ -> Dynamic (Option.isSome <!> !. ifModifiedSince_))
                        (s, ifModifiedSinceMatchesDecision p s)

                // TODO: Logic

                and private ifModifiedSinceMatchesDecision p s =
                    decision (key p, "if-modified-since-matches-decision")
                        (function | _ -> Static true)
                        (notModifiedTerminal p, s)

                (* Export *)

                let export =
                    hasIfNoneMatchDecision

            (* Unsafe *)

            module Unsafe =

                (* Key *)

                let private key p =
                    Key.add [ "unsafe" ] (key p)

                (* Decisions *)

                let private ifNoneMatch_ =
                        Request.Headers.ifNoneMatch_

                let private eTags_ =
                        Properties.Resource.eTags_

                // TODO: Logic

                let rec private hasIfNoneMatchDecision p s =
                    decision (key p, "has-if-none-match-decision")
                        (function | _ -> Static true)
                        (s, ifNoneMatchMatchesDecision p s)

                // TODO: Logic

                and private ifNoneMatchMatchesDecision p s =
                    decision (key p, "if-none-match-matches-decision")
                        (function | _ -> Static true)
                        (preconditionFailedTerminal p, s)

                (* Export *)

                let export =
                    hasIfNoneMatchDecision

        (* Conflict

           Decision determining whether the requested operation would cause a
           conflict given the current state of the resource.

           Where a conflict would be caused, a 409 response is returned,
           signalling a client error. *)

        [<RequireQualifiedAccess>]
        module Conflict =

            (* Key *)

            let private key p =
                Key.add [ p; "conflict" ] key

            (* Types *)

            type private Conflict =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Decisions = Decisions.empty
                      Terminals = Terminals.empty }

             and private Decisions =
                { Conflict: Value<bool> option }

                static member conflict_ =
                    (fun x -> x.Conflict), (fun e x -> { x with Decisions.Conflict = e })

                static member empty =
                    { Conflict = None }

             and private Terminals =
                { Conflict: Freya<unit> option }

                static member conflict_ =
                    (fun x -> x.Conflict), (fun e x -> { x with Terminals.Conflict = e })

                static member empty =
                    { Conflict = None }

            (* Optics *)

            let private conflict_ =
                Configuration.element_ Conflict.empty "conflict"

            (* Terminals *)

            let private terminals_ =
                    conflict_
                >-> Conflict.terminals_

            let conflictTerminal_ =
                    terminals_
                >-> Terminals.conflict_

            let private conflictTerminal p =
                terminal (key p, "conflict-terminal")
                    (Terminal.fromConfigurationWithOperation conflictTerminal_ Operations.conflict)

            (* Decisions*)

            let private decisions_ =
                    conflict_
                >-> Conflict.decisions_

            let conflictDecision_ =
                    decisions_
                >-> Decisions.conflict_

            let private conflictDecision p s =
                decision (key p, "conflict-decision")
                    (Decision.fromConfigurationOrFalse conflictDecision_)
                    (s, conflictTerminal p)

            (* Export *)

            let export =
                conflictDecision

        (* Operation *)

        [<RequireQualifiedAccess>]
        module Operation =

            (* Key *)

            let private key p =
                Key.add [ p; "operation" ] key

            (* Types *)

            type private Operation =
                { Operations: Operations
                  Decisions: Decisions
                  Terminals: Terminals }

                static member operations_ =
                    (fun x -> x.Operations), (fun o x -> { x with Operation.Operations = o })

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Operations = Operations.empty
                      Decisions = Decisions.empty
                      Terminals = Terminals.empty }

             and private Operations =
                { Operations: Map<Method,Freya<bool>> }

                static member operations_ =
                    (fun x -> x.Operations), (fun o x -> { x with Operations.Operations = o })

                static member empty =
                    { Operations = Map.empty }

             and private Decisions =
                { Completed: Value<bool> option }

                static member completed_ =
                    (fun x -> x.Completed), (fun c x -> { x with Completed = c })

                static member empty =
                    { Completed = None }

             and private Terminals =
                { InternalServerError: Freya<unit> option
                  Accepted: Freya<unit> option }

                static member internalServerError_ =
                    (fun x -> x.InternalServerError), (fun i x -> { x with InternalServerError = i })

                static member accepted_ =
                    (fun x -> x.Accepted), (fun a x -> { x with Accepted = a })

                static member empty =
                    { InternalServerError = None
                      Accepted = None }

            (* Optics *)

            let private operation_ =
                Configuration.element_ Operation.empty "operation"

            (* Terminals *)

            let private terminals_ =
                    operation_
                >-> Operation.terminals_

            let internalServerErrorTerminal_ =
                    terminals_
                >-> Terminals.internalServerError_

            let acceptedTerminal_ =
                    terminals_
                >-> Terminals.accepted_

            let private internalServerErrorTerminal p =
                terminal (key p, "internal-server-error-terminal")
                    (Terminal.fromConfigurationWithOperation internalServerErrorTerminal_ Operations.internalServerError)

            let private acceptedTerminal p =
                terminal (key p, "accepted-terminal")
                    (Terminal.fromConfigurationWithOperation acceptedTerminal_ Operations.accepted)

            (* Decisions *)

            let private operations_ =
                    operation_
                >-> Operation.operations_

            let operationMethod_ m =
                    operations_
                >-> Operations.operations_
                >-> Map.value_ m

            let private decisions_ =
                    operation_
                >-> Operation.decisions_

            let private completedDecision_ =
                    decisions_
                >-> Decisions.completed_

            let rec private operationDecision p m s =
                decision (key p, "operation-decision")
                    (function | Get (operationMethod_ m) (Some f) -> Dynamic (f)
                              | _ -> Static true)
                    (internalServerErrorTerminal p, completedDecision p s)

            and private completedDecision p s =
                decision (key p, "accepted-decision")
                    (Decision.fromConfigurationOrFalse completedDecision_)
                    (s, acceptedTerminal p)

            (* Export *)

            let export =
                operationDecision

        (* Responses *)

        [<RequireQualifiedAccess>]
        module Responses =

            (* Key *)

            let private key p =
                Key.add [ p; "responses" ] key

            (* Common *)

            [<RequireQualifiedAccess>]
            module Common =

                (* Key *)

                let private key p =
                    Key.add [ "common" ] (key p)

                (* Types *)

                type private Common =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { NoContent: Value<bool> option }

                    static member noContent_ =
                        (fun x -> x.NoContent), (fun n x -> { x with Decisions.NoContent = n })

                    static member empty =
                        { NoContent = None }

                 and private Terminals =
                    { NoContent: Freya<unit> option
                      Ok: Freya<unit> option }

                    static member noContent_ =
                        (fun x -> x.NoContent), (fun n x -> { x with Terminals.NoContent = n })

                    static member ok_ =
                        (fun x -> x.Ok), (fun o x -> { x with Ok = o })

                    static member empty =
                        { NoContent = None
                          Ok = None }

                (* Optics *)

                let private common_ =
                        Configuration.element_ Common.empty "responses.common"

                (* Terminals *)

                let private terminals_ =
                        common_
                    >-> Common.terminals_

                let noContentTerminal_ =
                        terminals_
                    >-> Terminals.noContent_

                let okTerminal_ =
                        terminals_
                    >-> Terminals.ok_

                let private noContentTerminal p =
                    terminal (key p, "no-content-terminal")
                        (Terminal.fromConfigurationWithOperation noContentTerminal_ Operations.noContent)

                let private okTerminal p =
                    terminal (key p, "ok-terminal")
                        (Terminal.fromConfigurationWithOperation okTerminal_ Operations.ok)

                (* Decisions *)

                let private decisions_ =
                        common_
                    >-> Common.decisions_

                let noContentDecision_ =
                        decisions_
                    >-> Decisions.noContent_

                let private noContentDecision p =
                    decision (key p, "no-content-decision")
                        (Decision.fromConfigurationOrFalse noContentDecision_)
                        (okTerminal p, noContentTerminal p)

                (* Export *)

                let export =
                    noContentDecision

            (* Created *)

            [<RequireQualifiedAccess>]
            module Created =

                (* Key *)

                let private key p =
                    Key.add [ "created" ] (key p)

                (* Types *)

                type private Created =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { Created: Value<bool> option }

                    static member created_ =
                        (fun x -> x.Created), (fun e x -> { x with Decisions.Created = e })

                    static member empty =
                        { Created = None }

                 and private Terminals =
                    { Created: Freya<unit> option }

                    static member created_ =
                        (fun x -> x.Created), (fun e x -> { x with Terminals.Created = e })

                    static member empty =
                        { Created = None }

                (* Optics *)

                let private created_ =
                    Configuration.element_ Created.empty "responses.created"

                (* Terminals *)

                let private terminals_ =
                        created_
                    >-> Created.terminals_

                let createdTerminal_ =
                        terminals_
                    >-> Terminals.created_

                let private createdTerminal p =
                    terminal (key p, "created-terminal")
                        (Terminal.fromConfigurationWithOperation createdTerminal_ Operations.created)

                (* Decisions*)

                let private decisions_ =
                        created_
                    >-> Created.decisions_

                let createdDecision_ =
                        decisions_
                    >-> Decisions.created_

                let private createdDecision p s =
                    decision (key p, "created-decision")
                        (Decision.fromConfigurationOrFalse createdDecision_)
                        (s, createdTerminal p)

                (* Export *)

                let export =
                    createdDecision

            (* Missing *)

            [<RequireQualifiedAccess>]
            module Missing =

                (* Key *)

                let private key p =
                    Key.add [ "missing" ] (key p)

                (* Types *)

                type private Missing =
                    { Terminals: Terminals }

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Terminals = Terminals.empty }

                 and private Terminals =
                    { NotFound: Freya<unit> option }

                    static member notFound_ =
                        (fun x -> x.NotFound), (fun n x -> { x with NotFound = n })

                    static member empty =
                        { NotFound = None }

                (* Optics *)

                let private missing_ =
                    Configuration.element_ Missing.empty "responses.missing"

                (* Terminals *)

                let private terminals_ =
                        missing_
                    >-> Missing.terminals_

                let notFoundTerminal_ =
                        terminals_
                    >-> Terminals.notFound_

                let private notFoundTerminal p =
                    terminal (key p, "not-found-terminal")
                        (Terminal.fromConfigurationWithOperation notFoundTerminal_ Operations.notFound)

                (* Export *)

                let export =
                    notFoundTerminal

            (* Moved *)

            [<RequireQualifiedAccess>]
            module Moved =

                (* Key *)

                let private key p =
                    Key.add [ "other" ] (key p)

                (* Types *)

                type private Moved =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { Gone: Value<bool> option
                      MovedTemporarily: Value<bool> option
                      MovedPermanently: Value<bool> option }

                    static member gone_ =
                        (fun x -> x.Gone), (fun g x -> { x with Decisions.Gone = g })

                    static member movedTemporarily_ =
                        (fun x -> x.MovedTemporarily), (fun m x -> { x with MovedTemporarily = m })

                    static member movedPermanently_ =
                        (fun x -> x.MovedPermanently), (fun m x -> { x with Decisions.MovedPermanently = m })

                    static member empty =
                        { Gone = None
                          MovedTemporarily = None
                          MovedPermanently = None }

                 and private Terminals =
                    { Gone: Freya<unit> option
                      TemporaryRedirect: Freya<unit> option
                      MovedPermanently: Freya<unit> option }

                    static member gone_ =
                        (fun x -> x.Gone), (fun g x -> { x with Terminals.Gone = g })

                    static member temporaryRedirect_ =
                        (fun x -> x.TemporaryRedirect), (fun t x -> { x with TemporaryRedirect = t })

                    static member movedPermanently_ =
                        (fun x -> x.MovedPermanently), (fun m x -> { x with Terminals.MovedPermanently = m })

                    static member empty =
                        { Gone = None
                          TemporaryRedirect = None
                          MovedPermanently = None }

                (* Optics *)

                let private moved_ =
                        Configuration.element_ Moved.empty "responses.moved"

                (* Terminals *)

                let private terminals_ =
                        moved_
                    >-> Moved.terminals_

                let goneTerminal_ =
                        terminals_
                    >-> Terminals.gone_

                let temporaryRedirectTemporal_ =
                        terminals_
                    >-> Terminals.temporaryRedirect_

                let movedPermanentlyTerminal_ =
                        terminals_
                    >-> Terminals.movedPermanently_

                let private goneTerminal p =
                    terminal (key p, "gone-terminal")
                        (Terminal.fromConfigurationWithOperation goneTerminal_ Operations.gone)

                let private temporaryRedirectTerminal_ p =
                    terminal (key p, "temporary-redirect-terminal")
                        (Terminal.fromConfigurationWithOperation temporaryRedirectTemporal_ Operations.temporaryRedirect)

                let private movedPermanentlyTerminal p =
                    terminal (key p, "moved-permanently-terminal")
                        (Terminal.fromConfigurationWithOperation movedPermanentlyTerminal_ Operations.movedPermanently)

                (* Decisions *)

                let private decisions_ =
                        moved_
                    >-> Moved.decisions_

                let goneDecision_ =
                        decisions_
                    >-> Decisions.gone_

                let movedTemporarilyDecision_ =
                        decisions_
                    >-> Decisions.movedTemporarily_

                let movedPermanentlyDecision_ =
                        decisions_
                    >-> Decisions.movedPermanently_

                let rec private goneDecision p s =
                    decision (key p, "see-other-decision")
                        (Decision.fromConfigurationOrFalse goneDecision_)
                        (movedTemporarilyDecision p s, goneTerminal p)

                and private movedTemporarilyDecision p s =
                    decision (key p, "found-decision")
                        (Decision.fromConfigurationOrFalse movedTemporarilyDecision_)
                        (movedPermanentlyDecision p s, temporaryRedirectTerminal_ p)

                and private movedPermanentlyDecision p s =
                    decision (key p, "see-other-decision")
                        (Decision.fromConfigurationOrFalse movedPermanentlyDecision_)
                        (s, movedPermanentlyTerminal p)

                (* Export *)

                let export =
                    goneDecision

            (* Options *)

            [<RequireQualifiedAccess>]
            module Options =

                (* Key *)

                let private key p =
                    Key.add [ "options" ] (key p)

                 (* Types *)

                type private Options =
                    { Terminals: Terminals }

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Terminals = Terminals.empty }

                 and private Terminals =
                    { Options: Freya<unit> option }

                    static member options_ =
                        (fun x -> x.Options), (fun n x -> { x with Options = n })

                    static member empty =
                        { Options = None }

                (* Optics *)

                let private options_ =
                    Configuration.element_ Options.empty "responses.options"

                (* Terminals *)

                let private terminals_ =
                        options_
                    >-> Options.terminals_

                let optionsTerminal_ =
                        terminals_
                    >-> Terminals.options_

                let private optionsTerminal p =
                    terminal (key p, "options-terminal")
                        (Terminal.fromConfigurationWithOperation optionsTerminal_ Operations.options)

                (* Export *)

                let export =
                    optionsTerminal

            (* Other *)

            [<RequireQualifiedAccess>]
            module Other =

                (* Key *)

                let private key p =
                    Key.add [ "other" ] (key p)

                (* Types *)

                type private Other =
                    { Decisions: Decisions
                      Terminals: Terminals }

                    static member decisions_ =
                        (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                    static member terminals_ =
                        (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                    static member empty =
                        { Decisions = Decisions.empty
                          Terminals = Terminals.empty }

                 and private Decisions =
                    { SeeOther: Value<bool> option
                      Found: Value<bool> option
                      MultipleChoices: Value<bool> option }

                    static member seeOther_ =
                        (fun x -> x.SeeOther), (fun s x -> { x with Decisions.SeeOther = s })

                    static member found_ =
                        (fun x -> x.Found), (fun f x -> { x with Decisions.Found = f })

                    static member multipleChoices_ =
                        (fun x -> x.MultipleChoices), (fun m x -> { x with Decisions.MultipleChoices = m })

                    static member empty =
                        { SeeOther = None
                          Found = None
                          MultipleChoices = None }

                 and private Terminals =
                    { SeeOther: Freya<unit> option
                      Found: Freya<unit> option
                      MultipleChoices: Freya<unit> option }

                    static member seeOther_ =
                        (fun x -> x.SeeOther), (fun s x -> { x with Terminals.SeeOther = s })

                    static member found_ =
                        (fun x -> x.Found), (fun f x -> { x with Terminals.Found = f })

                    static member multipleChoices_ =
                        (fun x -> x.MultipleChoices), (fun m x -> { x with Terminals.MultipleChoices = m })

                    static member empty =
                        { SeeOther = None
                          Found = None
                          MultipleChoices = None }

                (* Optics *)

                let private other_ =
                        Configuration.element_ Other.empty "responses.other"

                (* Terminals *)

                let private terminals_ =
                        other_
                    >-> Other.terminals_

                let seeOtherTerminal_ =
                        terminals_
                    >-> Terminals.seeOther_

                let foundTerminal_ =
                        terminals_
                    >-> Terminals.found_

                let multipleChoicesTerminal_ =
                        terminals_
                    >-> Terminals.multipleChoices_

                let private seeOtherTerminal p =
                    terminal (key p, "see-other-terminal")
                        (Terminal.fromConfigurationWithOperation seeOtherTerminal_ Operations.seeOther)

                let private foundTerminal p =
                    terminal (key p, "found-terminal")
                        (Terminal.fromConfigurationWithOperation foundTerminal_ Operations.found)

                let private multipleChoicesTerminal p =
                    terminal (key p, "multiple-choices-terminal")
                        (Terminal.fromConfigurationWithOperation multipleChoicesTerminal_ Operations.multipleChoices)

                (* Decisions *)

                let private decisions_ =
                        other_
                    >-> Other.decisions_

                let seeOtherDecision_ =
                        decisions_
                    >-> Decisions.seeOther_

                let foundDecision_ =
                        decisions_
                    >-> Decisions.found_

                let multipleChoicesDecision_ =
                        decisions_
                    >-> Decisions.multipleChoices_

                let rec private seeOtherDecision p s =
                    decision (key p, "see-other-decision")
                        (Decision.fromConfigurationOrFalse seeOtherDecision_)
                        (foundDecision p s, seeOtherTerminal p)

                and private foundDecision p s =
                    decision (key p, "found-decision")
                        (Decision.fromConfigurationOrFalse foundDecision_)
                        (multipleChoicesDecision p s, foundTerminal p)

                and private multipleChoicesDecision p s =
                    decision (key p, "see-other-decision")
                        (Decision.fromConfigurationOrFalse multipleChoicesDecision_)
                        (s, multipleChoicesTerminal p)

                (* Export *)

                let export =
                    seeOtherDecision

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
                terminal (key, "end-terminal")
                    (fun _ -> Operations.ok)

            (* Decisions *)

            let private endpointDecision =
                decision (key, "end-decision")
                    (fun _ -> Static true)
                    (Specification.Terminal.empty, endpointTerminal)

            (* Export *)

            let private core =
                Assertion.export Core (
                    Permission.export Core (
                        Validation.export Core (
                            Negotiation.export Core endpointDecision)))

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

            (* Export *)

            let private getOrHead s =
                Method.export GetOrHead [ GET; HEAD ] (
                    s, Existence.export GetOrHead (
                        Responses.Moved.export GetOrHead (
                            Responses.Missing.export GetOrHead),
                        Preconditions.Common.export GetOrHead (
                            Preconditions.Safe.export GetOrHead (
                                Responses.Other.export GetOrHead (
                                    Responses.Common.export GetOrHead)))))

            let export =
                { Metadata =
                    { Name = "http.get"
                      Description = None }
                  Requirements =
                    { Required = set [ "http.core" ]
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "end-decision" ], Right, getOrHead) ] }

        (* Options *)

        [<RequireQualifiedAccess>]
        module Options =

            [<Literal>]
            let private Options =
                "options"

            (* Export *)

            let private options s =
                Method.export Options [ OPTIONS ] (
                    s, Responses.Options.export Options)

            let export =
                { Metadata =
                    { Name = "http.options"
                      Description = None }
                  Requirements =
                    { Required = set [ "http.core" ]
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "end-decision" ], Right, options) ] }

        (* Post *)

        [<RequireQualifiedAccess>]
        module Post =

            [<Literal>]
            let private Post =
                "post"

            (* Export *)

            let private post s =
                Method.export Post [ POST ] (
                    s, Existence.export Post (
                        Responses.Moved.export Post (
                            Responses.Missing.export Post),
                        Preconditions.Common.export Post (
                            Preconditions.Unsafe.export Post (
                                Conflict.export Post (
                                    Operation.export Post POST (
                                        Responses.Created.export Post (
                                            Responses.Other.export Post (
                                                Responses.Common.export Post))))))))

            let export =
                { Metadata =
                    { Name = "http.post"
                      Description = None }
                  Requirements =
                    { Required = set [ "http.core" ]
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "end-decision" ], Right, post) ] }

        (* Put *)

        [<RequireQualifiedAccess>]
        module Put =

            [<Literal>]
            let private Put =
                "put"

            (* Export *)

            let rec private put s =
                Method.export Put [ PUT ] (
                    s, Existence.export Put (
                        Responses.Moved.export Put (
                            continuation),
                        Preconditions.Common.export Put (
                            Preconditions.Unsafe.export Put (
                                Conflict.export Put (
                                    continuation)))))

            and private continuation =
                Operation.export Put PUT (
                    Responses.Created.export Put (
                        Responses.Other.export Put (
                            Responses.Common.export Put)))

            let export =
                { Metadata =
                    { Name = "http.put"
                      Description = None }
                  Requirements =
                    { Required = set [ "http.core" ]
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "end-decision" ], Right, put) ] }

        (* Delete *)

        [<RequireQualifiedAccess>]
        module Delete =

            [<Literal>]
            let private Delete =
                "delete"

            (* Export *)

            let private delete s =
                Method.export Delete [ DELETE ] (
                    s, Existence.export Delete (
                        Responses.Moved.export Delete (
                            Responses.Missing.export Delete),
                        Preconditions.Common.export Delete (
                            Preconditions.Unsafe.export Delete (
                                Operation.export Delete DELETE (
                                    Responses.Common.export Delete)))))

            let export =
                { Metadata =
                    { Name = "http.delete"
                      Description = None }
                  Requirements =
                    { Required = set [ "http.core" ]
                      Preconditions = List.empty }
                  Operations =
                    [ Splice (Key [ "http"; "end-decision" ], Right, delete) ] }

    (* Model *)

    let model =
        Model.create (
            set [
                Core.export
                GetOrHead.export
                Options.export
                Post.export
                Put.export
                Delete.export ])

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

            let (Log.Graph (nodes, edges)) =
                machineLog.Optimization.Graphs.Post

            printfn "Nodes:\n"

            nodes
            |> List.iter (fun (Key k, _) ->
                printfn "%s" (String.Join (".", k))) 

            printfn "\nEdges:\n"

            edges
            |> List.rev
            |> List.iter (fun (Key k1, Key k2, v) ->
                printfn "%s -- %A --> %s" (String.Join (".", k1)) v (String.Join (".", k2)))

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

    (* Freya<bool> *)

    module Bool =

        type Defaults =
            | Defaults

            static member Bool (x: Freya<bool>) =
                x

            static member Bool (x: Freya<unit>) =
                Freya.map (x, fun _ -> true)

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Bool: ^a -> Freya<bool>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline bool v =
        Bool.infer v

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

(* Properties

   Configuration for common properties of a the HTTP model which may be used by
   multiple elements/components as part. Multiple implementations may rely on
   the same core declarative property of a resource without needing to be aware
   of the existence of other consumers of that property. *)

(* Request *)

type HttpMachineBuilder with

    [<CustomOperation ("methodsAllowed", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.MethodsAllowed (m, a) =
        HttpMachine.Map (m, Optic.set Model.Properties.Request.methodsAllowed_ (Some (Infer.methods a)))

(* Representation *)

type HttpMachineBuilder with

    [<CustomOperation ("mediaTypesSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.MediaTypesSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Properties.Representation.mediaTypesSupported_ (Some (Infer.mediaTypes a)))

(* Elements

   Configuration for discrete elements used to make up specific components used
   within the HTTP model. *)

(* Assertion *)

type HttpMachineBuilder with

    (* Decisions *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Assertion.serviceAvailableDecision_ (Some (Infer.value a)))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Assertion.httpVersionSupportedDecision_ (Some (Infer.value a)))

    (* Terminals *)

    [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleServiceUnavailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Assertion.serviceUnavailableTerminal_ (Some a))

    [<CustomOperation ("handleNotImplemented", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleNotImplemented (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Assertion.notImplementedTerminal_ (Some a))

    [<CustomOperation ("handleNotSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleNotSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Assertion.httpVersionNotSupportedTerminal_ (Some a))

(* Permission *)

type HttpMachineBuilder with

    (* Decisions *)

    [<CustomOperation ("authorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Authorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Permission.authorizedDecision_ (Some (Infer.value a)))

    [<CustomOperation ("allowed", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Allowed (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Permission.allowedDecision_ (Some (Infer.value a)))

    (* Terminals *)

    [<CustomOperation ("handleUnauthorized", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleUnauthorized (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Permission.unauthorizedTerminal_ (Some a))

    [<CustomOperation ("handleForbidden", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleForbidden (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Permission.forbiddenTerminal_ (Some a))

(* Operation *)

type HttpMachineBuilder with

    (* Operations *)

    [<CustomOperation ("doDelete", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.DoDelete (m, a) =
        HttpMachine.Map (m, Optic.set (Model.Elements.Operation.operationMethod_ DELETE) (Some (Infer.bool a)))

    [<CustomOperation ("doPost", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.DoPost (m, a) =
        HttpMachine.Map (m, Optic.set (Model.Elements.Operation.operationMethod_ POST) (Some (Infer.bool a)))

    [<CustomOperation ("doPut", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.DoPut (m, a) =
        HttpMachine.Map (m, Optic.set (Model.Elements.Operation.operationMethod_ PUT) (Some (Infer.bool a)))

(* Responses *)

type HttpMachineBuilder with

    (* Common *)

    (* Terminals *)

    [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleOk (m, a) =
        HttpMachine.Map (m, Optic.set Model.Elements.Responses.Common.okTerminal_ (Some (Infer.freya a)))

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
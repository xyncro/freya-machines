module Freya.Machines.Http

open System
open Aether
open Aether.Operators
open Arachne.Http
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

    (* MediaType *)

    [<RequireQualifiedAccess>]
    module MediaType =

        let negotiate supported acceptable =
            Option.map (function | Accept x -> x) acceptable
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

            let plus x =
                Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) [ x ])

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let inline orLiteral o l =
                function | TryGet o x -> x
                         | _ -> Value.Literal l

            let inline orTrue o =
                orLiteral o true

            let inline orFalse o =
                orLiteral o false

        (* Terminals *)

        [<RequireQualifiedAccess>]
        module Terminal =

            let inline from o operation =
                function | TryGet o x -> x *> operation
                         | _ -> operation

        let internal decision (key, name) configurator (l, r) =
            Specification.Decision.create (Key.plus name key) (configurator >> Decision.map) (l, r)

        let internal terminal (key, name) configurator =
            Specification.Terminal.create (Key.plus name key) configurator


    (* Key *)

    let private key =
        Key.plus "http" Key.empty

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key =
            Key.plus "common" key

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
                { MediaTypesSupported: Value<MediaType list> option }

                static member mediaTypesSupported_ =
                    (fun x -> x.MediaTypesSupported), (fun m x -> { x with MediaTypesSupported = m })

                static member empty =
                    { MediaTypesSupported = None }

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
                Specification.Terminal.create
                    (Key.plus "ok" key)
                    (Terminal.from ok_ Operations.ok)

    (* Components *)

    [<RequireQualifiedAccess>]
    module Components =

       // TODO: Consider whether fixed keys is appropriate here

        let private key =
            Key.plus "components" key

        [<RequireQualifiedAccess>]
        module Common =

            let private key =
                Key.plus "common" key

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
                    Key.plus "method" key

                let internal methodMatches m (l, r) =
                    decision (key, "method-matches-decision")
                        (fun _ -> Value.Function ((=) m <!> !. Request.method_))
                        (l, r)

            [<RequireQualifiedAccess>]
            module Existence =

                let private key =
                    Key.plus "existence" key

                let exists_ =
                        Configuration.common_
                    >-> Configuration.Common.existence_
                    >-> Configuration.Existence.exists_

                let internal exists (l, r) =
                    decision (key, "exists-decision")
                        (Decision.orTrue exists_)
                        (l, r)

    (* Core *)

    [<RequireQualifiedAccess>]
    module Core =

        let private key =
            Key.plus "core" key

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
                Key.plus "server" key

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
                        (Terminal.from serviceUnavailable_ Operations.serviceUnavailable)

                let internal httpVersionNotSupported =
                    terminal (key, "http-version-not-supported-terminal")
                        (Terminal.from httpVersionNotSupported_ Operations.httpVersionNotSupported)

                let internal notImplemented =
                    terminal (key, "not-implemented-terminal")
                        (Terminal.from notImplemented_ Operations.notImplemented)

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
                        (Decision.orTrue serviceAvailable_)
                        (Terminals.serviceUnavailable, httpVersionSupported s)

                and internal httpVersionSupported s =
                    decision (key, "http-version-supported-decision")
                        (Decision.orTrue httpVersionSupported_)
                        (Terminals.httpVersionNotSupported, methodImplemented s)

                // TODO: Not Implemented Logic

                and internal methodImplemented s =
                    decision (key, "method-implemented-decision")
                        (fun _ -> Value.Literal true)
                        (Terminals.notImplemented, s)

            (* Root *)

            let internal root =
                Decisions.serviceAvailable

        (* Core.Client *)

        [<RequireQualifiedAccess>]
        module Client =

            let private key =
                Key.plus "client" key

            let private client_ =
                    Configuration.core_
                >-> Configuration.Core.client_

            (* Core.Client.Access *)

            [<RequireQualifiedAccess>]
            module Access =

                let private key =
                    Key.plus "access" key

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
                            (Terminal.from unauthorized_ Operations.unauthorized)

                    let internal forbidden =
                        terminal (key, "forbidden-terminal")
                            (Terminal.from forbidden_ Operations.forbidden)

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
                            (Decision.orTrue authorized_)
                            (Terminals.unauthorized, allowed s)

                    and internal allowed s =
                        decision (key, "allowed-decision")
                            (Decision.orTrue allowed_)
                            (Terminals.forbidden, s)

                (* Root *)

                let internal root =
                    Decisions.authorized

            (* Core.Client.Request *)

            [<RequireQualifiedAccess>]
            module Request =

                let private key =
                    Key.plus "request" key

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
                            (Terminal.from expectationFailed_ Operations.expectationFailed)

                    let internal methodNotAllowed =
                        terminal (key, "method-not-allowed-terminal")
                            (Terminal.from methodNotAllowed_ Operations.methodNotAllowed)

                    let internal uriTooLong =
                        terminal (key, "uri-too-long-terminal")
                            (Terminal.from uriTooLong_ Operations.uriTooLong)

                    let internal badRequest =
                        terminal (key, "bad-request-terminal")
                            (Terminal.from badRequest_ Operations.badRequest)

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
                            (Decision.orTrue expectationMet_)
                            (Terminals.expectationFailed, methodAllowed s)

                    and internal methodAllowed s =
                        decision (key, "method-allowed-decision")
                            (Decision.orTrue methodAllowed_)
                            (Terminals.methodNotAllowed, uriTooLong s)

                    and internal uriTooLong s =
                        decision (key, "uri-too-long-decision")
                            (Decision.orTrue uriTooLong_)
                            (Terminals.uriTooLong, badRequest s)

                    and internal badRequest s =
                        decision (key, "bad-request-decision")
                            (Decision.orTrue badRequest_)
                            (Terminals.badRequest, s)

                (* Root *)

                let internal root =
                    Decisions.expectationMet

            (* Core.Client.Acceptable *)

            [<RequireQualifiedAccess>]
            module Acceptable =

                let private key =
                    Key.plus "acceptable" key

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
                            (Terminal.from notAcceptable_ Operations.notAcceptable)

                (* Decisions *)

                module Decisions =

                    let private accept_ =
                            Request.Headers.accept_

                    let private mediaTypesSupported_ =
                            Common.Properties.mediaTypesSupported_

                    let rec internal hasAccept s =
                        decision (key, "has-accept-decision")
                            (fun _ -> Value.Function (Option.isSome <!> !. accept_))

                    and internal acceptMatches s =
                        decision (key, "accept-matches-decision")
                            (function | TryGet mediaTypesSupported_ (Value.Function m) -> Value.Function (MediaType.negotiable <!> m <*> !. accept_)
                                      | TryGet mediaTypesSupported_ (Value.Literal m) -> Value.Function (MediaType.negotiable m <!> !. accept_)
                                      | _ -> Value.Literal true)

        (* Component *)

        let private specification =
                Server.root
             >> Client.Access.root
             >> Client.Request.root

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

    (* Value<bool> *)

    module Value =

        type Defaults =
            | Defaults

            static member Value (x: Freya<bool>) =
                Value.Function x

            static member Value (x: bool) =
                Value.Literal x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Value: ^a -> Value<bool>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline value v =
        Value.infer v

    (* Method list *)

    module Methods =

        type Defaults =
            | Defaults

            static member inline Methods (x: Freya<Method list>) =
                Value.Function x

            static member inline Methods (x: Method list) =
                Value.Literal x

            static member inline Methods (x: Method) =
                Value.Literal [ x ]

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
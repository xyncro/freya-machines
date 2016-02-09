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

    (* 5xx *)

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

    (* Keys

       Functions for working with Hephaestus Keys, making defining and using
       keys slightly more pleasant. The default empty key is included here
       for consistency at the various levels. *)

    let private keyWith x =
        Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) [ x ])

    let private key =
        Key.empty

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key =
            keyWith "common" key

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module private Configuration =

            (* Types *)

            type Common =
                { Terminals: Terminals }

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Terminals = Terminals.empty }

             and Terminals =
                { Ok: Freya<unit> option }

                static member ok_ =
                    (fun x -> x.Ok), (fun o x -> { x with Ok = o })

                static member empty =
                    { Ok = None }

            (* Optics *)

            let common_ =
                Configuration.element_ "common" Common.empty

        [<RequireQualifiedAccess>]
        module Terminals =

            let private terminals_ =
                    Configuration.common_
                >-> Configuration.Common.terminals_

            let ok_ =
                    terminals_
                >-> Configuration.Terminals.ok_

            let internal ok =
                Specification.Terminal.create (keyWith "ok" key)
                    (function | TryGet ok_ x -> Operations.ok *> x
                              | _ -> Operations.ok)

    (* Core *)

    [<RequireQualifiedAccess>]
    module Core =

        (* Key *)

        let private key =
            keyWith "core" key

        (* Configuration *)

        [<RequireQualifiedAccess>]
        module private Configuration =

            (* Types *)

            type Core =
                { Server: Server }

                static member server_ =
                    (fun x -> x.Server), (fun s x -> { x with Server = s })

                static member empty =
                    { Server = Server.empty }

             and Server =
                { Decisions: Decisions
                  Terminals: Terminals }

                static member decisions_ =
                    (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

                static member terminals_ =
                    (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

                static member empty =
                    { Decisions = Decisions.empty
                      Terminals = Terminals.empty }

             and Decisions =
                { ServiceAvailable: Decision
                  HttpVersionSupported: Decision option }

                static member serviceAvailable_ =
                    (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

                static member httpVersionSupported_ =
                    (fun x -> x.HttpVersionSupported), (fun h x -> { x with HttpVersionSupported = h })

                static member empty =
                    { ServiceAvailable = Freya.Machines.Literal true
                      HttpVersionSupported = None }

             and Terminals =
                { ServiceUnavailable: Freya<unit> option
                  HttpVersionNotSupported: Freya<unit> option }

                static member serviceUnavailable_ =
                    (fun x -> x.ServiceUnavailable), (fun s x -> { x with ServiceUnavailable = s })

                static member httpVersionNotSupported_ =
                    (fun x -> x.HttpVersionNotSupported), (fun h x -> { x with HttpVersionNotSupported = h })

                static member empty =
                    { ServiceUnavailable = None
                      HttpVersionNotSupported = None }

            (* Optics *)

            let core_ =
                Configuration.element_ "core" Core.empty

        (* Server

           Core decisions and terminals for the Core.Server section of the HTTP
           machine. *)

        [<RequireQualifiedAccess>]
        module Server =

            (* Key *)

            let private key =
                keyWith "server" key

            (* Optics *)

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
                    >-> Configuration.Terminals.serviceUnavailable_

                let httpVersionNotSupported_ =
                        terminals_
                    >-> Configuration.Terminals.httpVersionNotSupported_

                let internal serviceUnavailable =
                    Specification.Terminal.create (keyWith "serviceUnavailable" key)
                        (function | TryGet serviceUnavailable_ x -> Operations.serviceUnavailable *> x
                                  | _ -> Operations.serviceUnavailable)

                let internal httpVersionNotSupported =
                    Specification.Terminal.create (keyWith "httpVersionNotSupported" key)
                        (function | TryGet httpVersionNotSupported_ x -> Operations.httpVersionNotSupported *> x
                                  | _ -> Operations.httpVersionNotSupported)

            (* Decisions *)

            [<RequireQualifiedAccess>]
            module Decisions =

                let private decisions_ =
                        server_
                    >-> Configuration.Server.decisions_

                let serviceAvailable_ =
                        decisions_
                    >-> Configuration.Decisions.serviceAvailable_

                let httpVersionSupported_ =
                        decisions_
                    >-> Configuration.Decisions.httpVersionSupported_

                let rec internal serviceAvailable s =
                    Specification.Decision.create (keyWith "serviceAvailable" key)
                        (function | Get serviceAvailable_ x -> Decision.map x)
                        (Terminals.serviceUnavailable, httpVersionSupported s)

                and internal httpVersionSupported s =
                    Specification.Decision.create (keyWith "httpVersionSupported" key)
                        (function | TryGet httpVersionSupported_ x -> Decision.map x
                                  | _ -> Decision.map (Freya.Machines.Literal true))
                        (Terminals.httpVersionNotSupported, s)

        (* Component *)

        let internal core =
            { Metadata =
                { Name = "core"
                  Description = None }
              Requirements =
                { Required = Set.empty
                  Preconditions = List.empty }
              Operations =
                [ Prepend (fun _ -> Server.Decisions.serviceAvailable Common.Terminals.ok) ] }

    (* Model *)

    let model =
        Model.create (set [ Core.core ])

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

        let private prototype =
            Prototype.create Model.model

        let reify configuration =
            Evaluation.evaluate (Machine.create prototype configuration)

(* Inference

   Type inference functions for conversion of various forms to a single form,
   for example the conversion of functions and literal values to be
   automatically inferred at compile time to be Literal or Function types of
   Decision.

   This gives a more flexible API where it is used, although at the cost of
   greater documentation/lesser discoverability initially. *)

[<RequireQualifiedAccess>]
module Infer =

    module Decision =

        type Defaults =
            | Defaults

            static member inline Decision (x: Freya<bool>) =
                Freya.Machines.Function x

            static member inline Decision (x: bool) =
                Freya.Machines.Literal x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Decision: ^a -> Decision) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline decision v =
        Decision.infer v

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

    (* Common.Terminals *)

    [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleOk (m, a) =
        HttpMachine.Map (m, Optic.set Model.Common.Terminals.ok_ (Some a))

    (* Core.Server.Decisions *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Decisions.serviceAvailable_ (Infer.decision a))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Decisions.httpVersionSupported_ (Some (Infer.decision a)))

    (* Core.Server.Terminals *)

    [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleServiceUnavailable (m, a) =
        HttpMachine.Map (m, Optic.set Model.Core.Server.Terminals.serviceUnavailable_ (Some a))

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
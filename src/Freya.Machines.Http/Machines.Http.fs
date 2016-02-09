module Freya.Machines.Http

open System
open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http

// TODO: Review comments, etc.
// TODO: Move __.Zero () to Core
// TODO: Defaults
// TODO: Operations
// TODO: Genuine Responses for Server
// TODO: Rest of Machine!

(* Operations

   Common operations for standard HTTP responses, setting various header values
   according to the appropriate logic for the response. These are commonly used
   by various terminals within the HTTP Machine. *)

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

(* Machine

   The definition and implementation of a general HTTP Machine, used by the
   Hephaestus library to construct and optimize a parameterized execution graph
   for an arbitrary binary decision machine.

   The Machine assumes Freya<bool> functions for decisions (for which a mapping
   is provided in the common Freya.Machines library) and Settings for the type
   of configuration, also provided along with utility tooling by the
   Freya.Machines library. *)

[<RequireQualifiedAccess>]
module Machine =

    (* Specification

       Helpful "opinionated" wrappers around the creation of decisions and
       terminals in the Hephaestus sense.

       Decisions are assumed to take a three part path, generally representing the
       element, the section within that element, and the name of the decision
       itself. *)

    [<RequireQualifiedAccess>]
    module private Specification =

        open Hephaestus

        let decision a b c configurator (left, right) =
            Specification.Decision.create (Key [ a; b; c ]) configurator (left, right)

        let terminal a b c f =
            Specification.Terminal.create (Key [ a; b; c ]) f

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Specifications *)

        let private terminal =
            Specification.terminal "common"

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

            let private terminal =
                terminal ""

            let private terminals_ =
                    Configuration.common_
                >-> Configuration.Common.terminals_

            let ok_ =
                    terminals_
                >-> Configuration.Terminals.ok_

            let internal ok =
                terminal "ok"
                    (function | TryGet ok_ x -> Operations.ok *> x
                              | _ -> Operations.ok)

    (* Core *)

    [<RequireQualifiedAccess>]
    module Core =

        (* Specifications *)

        let private decision =
            Specification.decision "core"

        let private terminal =
            Specification.terminal "core"

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
                    { ServiceAvailable = Literal true
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

            (* Optics *)

            let private server_ =
                    Configuration.core_
                >-> Configuration.Core.server_

            (* Terminals *)

            [<RequireQualifiedAccess>]
            module Terminals =

                let private terminal =
                    terminal "server"

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
                    terminal "serviceUnavailable"
                        (function | TryGet serviceUnavailable_ x -> Operations.serviceUnavailable *> x
                                  | _ -> Operations.serviceUnavailable)

                let internal httpVersionNotSupported =
                    terminal "httpVersionNotSupported"
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

                let private decision =
                    decision "server"

                let rec internal serviceAvailable s =
                    decision "serviceAvailable"
                        (function | Get serviceAvailable_ x -> Decision.map x)
                        (Terminals.serviceUnavailable, httpVersionSupported s)

                and internal httpVersionSupported s =
                    decision "httpVersionSupported"
                        (function | TryGet httpVersionSupported_ x -> Decision.map x
                                  | _ -> Decision.map (Literal true))
                        (Terminals.httpVersionNotSupported, s)

        (* Component *)

        open Hephaestus

        let internal core =
            { Metadata =
                { Name = "core"
                  Description = None }
              Requirements =
                { Required = Set.empty
                  Preconditions = List.empty }
              Operations =
                [ Prepend (fun _ -> Server.Decisions.serviceAvailable Common.Terminals.ok) ] }

    (* Execution *)

    [<RequireQualifiedAccess>]
    module Execution =

        open Hephaestus

        let private model =
            Model.create (set [ Core.core ])

        let private prototype =
            Prototype.create model

        let build (configure: Configuration -> unit * Configuration) =
            let configuration = snd (configure Configuration.empty)
            let machine = Machine.create prototype configuration

            ()

(* Inference

   Static type inference functions for automatic conversion of common function
   and literal values to their Decision<'a> form, allowing for a more
   expressive API within the computation expression (or other areas where
   configuration values may be supplied). *)

[<RequireQualifiedAccess>]
module Infer =

    module Decision =

        type Defaults =
            | Defaults

            static member inline Decision (x: Freya<bool>) =
                Function x

            static member inline Decision (x: bool) =
                Literal x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Decision: ^a -> Decision) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline decision v =
        Decision.infer v

(* Types

   The basic types of configuration, the HttpMachine (configuration) function,
   and the actual configuration data threaded through configuration functions.

   The function itself is defined as a single case discriminated union so that
   it can have static members, allowing it to take part in the static inference
   approaches of the basic Freya function, and Pipelines. *)

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

(* Builder

   Computation expression builder for configuring the HTTP Machine, providing a
   simple type-safe syntax and static inference based overloads of single
   functions. *)

type HttpMachineBuilder () =
    inherit Configuration.Builder<HttpMachine>
        { Init = HttpMachine.Init
          Bind = HttpMachine.Bind }

(* Syntax

   Extensions to the operations available as part of the defined Http Machine
   computation expression. *)

type HttpMachineBuilder with

    (* Common.Terminals *)

    [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleOk (m, a) =
        HttpMachine.Map (m, Optic.set Machine.Common.Terminals.ok_ (Some a))

    (* Core.Server.Decisions *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.Map (m, Optic.set Machine.Core.Server.Decisions.serviceAvailable_ (Infer.decision a))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.Map (m, Optic.set Machine.Core.Server.Decisions.httpVersionSupported_ (Some (Infer.decision a)))

    (* Core.Server.Terminals *)

    [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HandleServiceUnavailable (m, a) =
        HttpMachine.Map (m, Optic.set Machine.Core.Server.Terminals.serviceUnavailable_ (Some a))

(* Expressions

   Computation expressions, instances of the HTTP Machine builder. The fully
   named instance, freyaHttpMachine is aliased to freyaMachine to provide the
   possibility of more concise code when only one kind of machine is in scope.

   This naming also matches the original single form approach to machines, and
   provides backwards compatibility. *)

let freyaHttpMachine =
    HttpMachineBuilder ()

let freyaMachine =
    freyaHttpMachine
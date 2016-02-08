module Freya.Machines.Http

open Aether
open Aether.Operators
open Freya.Core

(* Types

   The basic types of configuration, the HttpMachine (configuration) function,
   and the actual configuration data threaded through configuration functions.

   The function itself is defined as a single case discriminated union so that
   it can have static members, allowing it to take part in the static inference
   approaches of the basic Freya function, and Pipelines. *)

type HttpMachine =
    | HttpMachine of (Configuration -> unit * Configuration)

(* Functions

   Functions implementing common monadic operations on the Machine type,
   which will be used to implement the computation expression builder.

   Additionally some extensions to the basic requirements for a computation
   expression are included, including mapping over the inner settings. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module HttpMachine =

    (* Common *)

    let init _ : HttpMachine =
        HttpMachine (fun c ->
            (), c)

    let bind (m: HttpMachine, f: unit -> HttpMachine) : HttpMachine =
        HttpMachine (fun c ->
            let (HttpMachine m) = m
            let (HttpMachine f) = f ()

            (), snd (f (snd (m c))))

    (* Custom *)

    let inline map (m: HttpMachine, f: Configuration -> Configuration) : HttpMachine =
        HttpMachine (fun c ->
            let (HttpMachine m) = m

            (), f (snd (m c)))

    (* Operations *)

    let operations =
        { Configuration.Operations.Init = init
          Configuration.Operations.Bind = bind }

(* Builder

   Computation expression builder for configuring the HTTP Machine, providing a
   simple type-safe syntax and static inference based overloads of single
   functions. *)

type HttpMachineBuilder () =
    inherit Configuration.Builder<HttpMachine> (HttpMachine.operations)

    member __.Zero () =
        HttpMachine.init ()

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

(* Specification

   Helpful "opinionated" wrappers around the creation of decisions and
   terminals in the Hephaestus sense.

   Decisions are assumed to take a three part path, generally representing the
   element, the section within that element, and the name of the decision
   itself. *)

[<RequireQualifiedAccess>]
module internal Specification =

    open Hephaestus

    let decision a b c configurator (left, right) =
        Specification.Decision.create (Key [ a; b; c ]) configurator (left, right)

    let terminal name =
        Specification.Terminal.create (Key [ name ]) (fun _ -> Freya.init name)

(* Machine

   The definition and implementation of a general HTTP Machine, used by the
   Hephaestus library to construct and optimize a parameterized execution graph
   for an arbitrary binary decision machine.

   The Machine assumes Freya<bool> functions for decisions (for which a mapping
   is provided in the common Freya.Machines library) and Settings for the type
   of configuration, also provided along with utility tooling by the
   Freya.Machines library. *)

(* Core *)

[<RequireQualifiedAccess>]
module Core =

    (* Specifications *)

    let private decision =
        Specification.decision "core"

    let private terminal =
        Specification.terminal

    (* Configuration *)

    [<RequireQualifiedAccess>]
    module private Configuration =

        type Core =
            { Server: Server }

            static member server_ =
                (fun x -> x.Server), (fun s x -> { x with Server = s })

            static member empty =
                { Server = Server.empty }

         and Server =
            { ServiceAvailable: Decision
              HttpVersionSupported: Decision option }

            static member serviceAvailable_ =
                (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

            static member httpVersionSupported_ =
                (fun x -> x.HttpVersionSupported), (fun h x -> { x with HttpVersionSupported = h })

            static member empty =
                { ServiceAvailable = Literal true
                  HttpVersionSupported = None }

        (* Optics *)

        let core_ =
            Configuration.element_ "core" Core.empty

    (* Server

       Core decisions and responses for the Core.Server section of the HTTP
       machine. *)

    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Server =

        (* Optics *)

        let private server_ =
                Configuration.core_
            >-> Configuration.Core.server_

        let serviceAvailable_ =
                server_
            >-> Configuration.Server.serviceAvailable_

        let httpVersionSupported_ =
                server_
            >-> Configuration.Server.httpVersionSupported_

        (* Specifications *)

        let private decision =
            decision "server"

        (* Decisions *)

        let rec internal serviceAvailable s =
            decision "serviceAvailable"
                (function | Get serviceAvailable_ x -> Decision.map x)
                (terminal "503", httpVersionSupported s)

        and internal httpVersionSupported s =
            decision "httpVersionSupported"
                (function | TryGet httpVersionSupported_ x -> Decision.map x
                          | _ -> Decision.map (Literal true))
                (terminal "505", s)

    (* Component *)

    open Hephaestus

    let internal export =
        { Metadata =
            { Name = "core"
              Description = None }
          Requirements =
            { Required = Set.empty
              Preconditions = List.empty }
          Operations =
            [ Prepend (fun _ -> Server.serviceAvailable (terminal "ok")) ] }

(* Syntax

    Extensions to the operations available as part of the defined Http Machine
    computation expression. *)

type HttpMachineBuilder with

    (* Core

       Syntax elements for the various configuration values used by parts
       making up the Core component. *)

    (* Server *)

    [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ServiceAvailable (m, a) =
        HttpMachine.map (m, Optic.set Core.Server.serviceAvailable_ (Infer.decision a))

    [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.HttpVersionSupported (m, a) =
        HttpMachine.map (m, Optic.set Core.Server.httpVersionSupported_ (Some (Infer.decision a)))
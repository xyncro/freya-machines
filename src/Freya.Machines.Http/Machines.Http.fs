module Freya.Machines.Http

open Aether
open Aether.Operators
open Freya.Core

(* Configuration

   Types and functions for user configuration of an HTTP Machine, based
   on configuration through a computation expression with various custom
   operations, and taking advantage of static type inference to allow a concise
   and flexible API (for example, accepting a Method or list of Methods
   transparently). *)

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

(* Machine

   The definition and implementation of a general HTTP Machine, used by the
   Hephaestus library to construct and optimize a parameterized execution graph
   for an arbitrary binary decision machine.

   The Machine assumes Freya<bool> functions for decisions (for which a mapping
   is provided in the common Freya.Machines library) and Settings for the type
   of configuration, also provided along with utility tooling by the
   Freya.Machines library. *)

[<RequireQualifiedAccess>]
module Core =

    type Core =
        { Server: Server }

        static member server_ =
            (fun x -> x.Server), (fun s x -> { x with Server = s })

        static member empty =
            { Server = Server.empty }

     and Server =
        { ServiceAvailable: Decision }

        static member serviceAvailable_ =
            (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

        static member empty =
            { ServiceAvailable = Literal true }

    (* Optics *)

    let core_ =
        Configuration.element_ "core" Core.empty

    (* Server

       Core decisions and responses for the Core.Server section of the HTTP
       machine. *)

    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Server =

        (* Optics *)

        let serviceAvailable_ =
                core_
            >-> Core.server_
            >-> Server.serviceAvailable_

        (* Syntax

           Extensions to the operations available as part of the freyaHttp
           computation expression. *)

        type HttpMachineBuilder with

            [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
            member inline __.ServiceAvailable (m, a) =
                HttpMachine.map (m, Optic.set serviceAvailable_ (Infer.decision a))
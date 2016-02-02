module Freya.Machines.Http

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
    | HttpMachine of (HttpMachineSettings -> unit * HttpMachineSettings)

 and HttpMachineSettings =
    | HttpMachineSettings of string list

    static member settings_ =
        (fun (HttpMachineSettings x) -> x), (HttpMachineSettings)

(* Functions

   Functions implementing common monadic operations on the Router type,
   which will be used to implement the computation expression builder.

   Additionally some extensions to the basic requirements for a computation
   expression are included, including mapping over the inner state. *)

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

    let inline map (m: HttpMachine, f: HttpMachineSettings -> HttpMachineSettings) : HttpMachine =
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


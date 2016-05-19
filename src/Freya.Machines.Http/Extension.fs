namespace Freya.Machines.Http

open Aether
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Models
open Hephaestus

(* Types

   The base type of an HTTP Machine, representing the user facing type defined
   through the use of the following computation expression.

   The function itself is defined as a single case discriminated union so that
   it can have static members, allowing it to take part in the static inference
   approaches of the basic Freya function, and Pipelines (allowing the pseudo
   typeclass approach which Freya uses in various places for concise APIs).

   The type is defined late to support extension for static inference. *)

type HttpMachine =
    | HttpMachine of (Configuration -> unit * Configuration)

(* HttpMachine *)

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

    let inline set (m: HttpMachine, o, v) =
        HttpMachine (fun c ->
            let (HttpMachine m) = m

            (), Optic.set o (Some v) (snd (m c)))

    (* Pipeline *)

    let private prototype =
        Prototype.create Http.model

    let internal pipeline (HttpMachine machine) : Pipeline =
        let configuration = snd (machine Configuration.empty)
        let machine = Machine.create prototype configuration

        Machine.execute machine *> Pipeline.next

(* Extensions *)

type HttpMachine with

    static member Freya machine : Freya<_> =
        HttpMachine.pipeline machine

    static member Pipeline machine : Pipeline =
        HttpMachine.pipeline machine
namespace Freya.Machines.Http

open System.Text
open Aether
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Machine.Models
open Hekate
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

    let inline map (m: HttpMachine, o, f) =
        HttpMachine (fun c ->
            let (HttpMachine m) = m

            (), Optic.map o f (snd (m c)))

    (* Pipeline *)

    let rec private log (s: StringBuilder) =
        function | Log ps -> passes s ps

    and private passes (s: StringBuilder) =
        function | p :: ps -> passes (pass s p) ps
                 | _ -> s

    and private pass (s: StringBuilder) =
        function | Pass (n, g, os) -> graph (operations (s.AppendFormat ("\n\nPass: {0}\n\n", n)) os) g

    and private graph (s: StringBuilder) =
        function | g -> edges (nodes s g) g

    and private nodes (s: StringBuilder) =
        function | g -> List.fold node (s.Append ("\n\nGraph:\n\n")) (Graph.Nodes.toList g)

    and private node (s: StringBuilder) =
        function | Key k, _ -> s.AppendFormat ("Node: {0}\n", String.concat "." k)

    and private edges (s: StringBuilder) =
        function | g -> List.fold edge s (Graph.Edges.toList g)

    and private edge (s: StringBuilder) =
        function | Key k1, Key k2, _ -> s.AppendFormat ("\"{0}\" -> \"{1}\"\n", (String.concat "." k1), (String.concat ".") k2)

    and private operations (s: StringBuilder) =
        function | o :: os -> operations (operation s o) os
                 | _ -> s

    and private operation (s: StringBuilder) =
        function | Operation (n, x) -> data (s.AppendFormat ("Operation: {0}\n", n)) x

    and private data (s: StringBuilder) =
        function | m -> s.AppendFormat ("Data: {0}\n\n", sprintf "%A" m)

    let internal pipeline (HttpMachine machine) : Pipeline =
        let configuration = snd (machine Configuration.empty)
        let extensions = Optic.get Extensions.Components.components_ configuration
        let model = Http.model extensions
        let prototype, prototypeLog = Prototype.createLogged model
        let machine, machineLog = Machine.createLogged prototype configuration

        let machineLogStr = string (log (StringBuilder ()) machineLog)

        Machine.execute machine *> Pipeline.next

(* Extensions *)

type HttpMachine with

    static member Freya machine : Freya<_> =
        HttpMachine.pipeline machine

    static member Pipeline machine : Pipeline =
        HttpMachine.pipeline machine
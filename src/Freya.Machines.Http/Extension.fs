namespace Freya.Machines.Http

open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Models
open Hephaestus

[<AutoOpen>]
module Extension =

    (* Functions *)

    let private prototype =
        Prototype.create Http.model

    let private execute (HttpMachine machine) =
        let configuration = snd (machine Configuration.empty)
        let machine = Machine.create prototype configuration

        fun state ->
            Machine.execute machine state

    (* Extensions *)

    type HttpMachine with

        static member Freya machine : Freya<_> =
            execute machine

        static member Pipeline machine : Pipeline =
            execute machine *> Pipeline.next
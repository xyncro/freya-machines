namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Freya.Machines
open Freya.Machines.Http.Machine.Specifications
open Freya.Machines.Http.Semantics
open Hephaestus

(* Core *)

[<RequireQualifiedAccess>]
module internal Core =

    (* Name *)

    [<Literal>]
    let private Core =
        "core"

    (* Terminals *)

    let private endpointTerminal =
        Terminal.create (Key.root, "end")
            (function | _ -> Operation.ok None None)
            (function | _ -> None)

    (* Decisions *)

    let private endpointDecision =
        Decision.create (Key.root, "end")
            (function | _ -> Static true)
            (Specification.Terminal.empty, endpointTerminal)

    (* Component *)

    let private core =
        Assertions.specification Core (
            Permissions.specification Core (
                Validations.specification Core (
                    Negotiations.specification Core endpointDecision)))

    let component =
        { Metadata = 
            { Name = "http.core"
              Description = None }
          Requirements =
            { Required = Set.empty
              Preconditions = List.empty }
          Operations =
            [ Prepend (fun _ -> core) ] }

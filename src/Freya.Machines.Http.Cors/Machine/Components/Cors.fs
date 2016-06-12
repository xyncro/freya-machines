namespace Freya.Machines.Http.Cors.Machine.Components

#nowarn "46"

open Freya.Core
open Freya.Machines
open Freya.Machines.Http.Machine.Components
open Freya.Machines.Http.Cors.Machine.Specifications
open Hephaestus

(* Cors *)

[<RequireQualifiedAccess>]
module Cors =

    (* Name *)

    [<Literal>]
    let Name =
        "http-cors"

    (* Component *)

    let private simple s =
        Simple.specification Name s

    let private preflight s =
        Preflight.specification Name s

    let component : Component<Configuration,unit,State> =
        { Metadata = 
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ "http-options" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Options.Name; "method"; "method-matches-decision" ], Left, simple)
              Splice (Key [ Options.Name; "method"; "method-matches-decision" ], Right, preflight) ] }

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
        "httpCors"

    (* Component *)

    let private simple =
        Simple.specification Name

    let private preflight =
        Preflight.specification Name

    let component : Component<Configuration,unit,State> =
        { Metadata = 
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ "httpOptions" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Options.Name; "method"; "methodMatches-decision" ], Left, simple)
              Splice (Key [ Options.Name; "method"; "methodMatches-decision" ], Right, preflight) ] }

namespace Freya.Machines.Http.Cors.Machine.Components

#nowarn "46"

open Freya.Core
open Freya.Machines
open Freya.Machines.Http.Cors.Machine.Specifications
open Hephaestus

(* Preflight *)

[<RequireQualifiedAccess>]
module internal Cors =

    (* Name *)

    [<Literal>]
    let Name =
        "http-cors"

    (* Component *)

    let private simple s =
        Simple.specification Name s

    let component : Component<Configuration,unit,State> =
        { Metadata = 
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ "http-options" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [], Left, simple) ] }

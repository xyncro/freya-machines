namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Freya.Machines.Http.Machine.Specifications
open Freya.Types.Http
open Hephaestus

(* Options *)

[<RequireQualifiedAccess>]
module Options =

    (* Name *)

    [<Literal>]
    let Name =
        "http-options"

    (* Component *)

    let private options s =
        Method.specification Name (set [ OPTIONS ]) (
            s, Responses.Options.specification Name)

    let component =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ Core.Name ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Core.Name; "validations"; "bad-request-decision" ], Left, options) ] }

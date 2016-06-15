namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Freya.Machines.Http.Machine.Specifications
open Freya.Types.Http
open Hephaestus

(* Delete *)

[<RequireQualifiedAccess>]
module Delete =

    (* Name *)

    [<Literal>]
    let Name =
        "http-delete"

    (* Component *)

    let private delete s =
        Method.specification Name (set [ DELETE ]) (
            s, Existence.specification Name (
                Responses.Moved.specification Name (
                    Responses.Missing.specification Name),
                Preconditions.Common.specification Name (
                    Preconditions.Unsafe.specification Name (
                        Operation.specification Name DELETE (
                            Responses.Common.specification Name)))))

    let component =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ Core.Name ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Core.Name; "fallback"; "fallback-decision" ], Right, delete) ] }
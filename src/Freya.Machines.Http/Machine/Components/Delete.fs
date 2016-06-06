namespace Freya.Machines.Http

#nowarn "46"

open Arachne.Http
open Hephaestus

(* Delete *)

[<RequireQualifiedAccess>]
module internal Delete =

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
                        Operations.specification Name DELETE (
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
namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Arachne.Http
open Freya.Machines.Http.Machine.Specifications
open Hephaestus

(* Put *)

[<RequireQualifiedAccess>]
module Put =

    (* Name *)

    [<Literal>]
    let Name =
        "http-put"

    (* Component *)

    let rec private put s =
        Method.specification Name (set [ PUT ]) (
            s, Existence.specification Name (
                Responses.Moved.specification Name (
                    continuation),
                Preconditions.Common.specification Name (
                    Preconditions.Unsafe.specification Name (
                        Conflict.specification Name (
                            continuation)))))

    and private continuation =
        Operation.specification Name PUT (
            Responses.Created.specification Name (
                Responses.Other.specification Name (
                    Responses.Common.specification Name)))

    let component =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ Core.Name ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Core.Name; "fallback"; "fallback-decision" ], Right, put) ] }

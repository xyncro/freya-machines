namespace Freya.Machines.Http

#nowarn "46"

open Arachne.Http
open Hephaestus

(* Put *)

[<RequireQualifiedAccess>]
module internal Put =

    (* Name *)

    [<Literal>]
    let private Put =
        "put"

    (* Component *)

    let rec private put s =
        Method.specification Put (set [ PUT ]) (
            s, Existence.specification Put (
                Responses.Moved.specification Put (
                    continuation),
                Preconditions.Common.specification Put (
                    Preconditions.Unsafe.specification Put (
                        Conflict.specification Put (
                            continuation)))))

    and private continuation =
        Operations.specification Put PUT (
            Responses.Created.specification Put (
                Responses.Other.specification Put (
                    Responses.Common.specification Put)))

    let component =
        { Metadata =
            { Name = "http.put"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ "http"; "core"; "fallback"; "fallback-decision" ], Right, put) ] }

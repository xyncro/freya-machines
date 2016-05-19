namespace Freya.Machines.Http

#nowarn "46"

open Arachne.Http
open Hephaestus

(* Delete *)

[<RequireQualifiedAccess>]
module internal Delete =

    (* Name *)

    [<Literal>]
    let private Delete =
        "delete"

    (* Component *)

    let private delete s =
        Method.specification Delete (Set.ofList [ DELETE ]) (
            s, Existence.specification Delete (
                Responses.Moved.specification Delete (
                    Responses.Missing.specification Delete),
                Preconditions.Common.specification Delete (
                    Preconditions.Unsafe.specification Delete (
                        Operations.specification Delete DELETE (
                            Responses.Common.specification Delete)))))

    let component =
        { Metadata =
            { Name = "http.delete"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ "http"; "end-decision" ], Right, delete) ] }
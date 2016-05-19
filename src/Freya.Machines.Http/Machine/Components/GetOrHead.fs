namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Arachne.Http
open Freya.Machines.Http.Machine.Specifications
open Hephaestus

(* Get or Head *)

[<RequireQualifiedAccess>]
module internal GetOrHead =

    (* Name *)

    [<Literal>]
    let private GetOrHead =
        "get-or-head"

    (* Component *)

    let private getOrHead s =
        Method.specification GetOrHead (set [ GET; HEAD ]) (
            s, Existence.specification GetOrHead (
                Responses.Moved.specification GetOrHead (
                    Responses.Missing.specification GetOrHead),
                Preconditions.Common.specification GetOrHead (
                    Preconditions.Safe.specification GetOrHead (
                        Responses.Other.specification GetOrHead (
                            Responses.Common.specification GetOrHead)))))

    let component =
        { Metadata =
            { Name = "http.get"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ "http"; "end-decision" ], Right, getOrHead) ] }
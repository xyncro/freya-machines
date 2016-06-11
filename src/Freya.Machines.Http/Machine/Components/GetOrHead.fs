namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Arachne.Http
open Freya.Machines.Http.Machine.Specifications
open Hephaestus

(* Get or Head *)

[<RequireQualifiedAccess>]
module GetOrHead =

    (* Name *)

    [<Literal>]
    let Name =
        "http-get-or-head"

    (* Component *)

    let private getOrHead s =
        Method.specification Name (set [ GET; HEAD ]) (
            s, Existence.specification Name (
                Responses.Moved.specification Name (
                    Responses.Missing.specification Name),
                Preconditions.Common.specification Name (
                    Preconditions.Safe.specification Name (
                        Responses.Other.specification Name (
                            Responses.Common.specification Name)))))

    let component =
        { Metadata =
            { Name = "http.get"
              Description = None }
          Requirements =
            { Required = set [ Core.Name ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Core.Name; "fallback"; "fallback-decision" ], Right, getOrHead) ] }
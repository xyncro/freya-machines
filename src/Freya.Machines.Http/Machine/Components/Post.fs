namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Freya.Machines.Http.Machine.Specifications
open Freya.Types.Http
open Hephaestus

(* Post *)

[<RequireQualifiedAccess>]
module Post =

    (* Name *)

    [<Literal>]
    let Name =
        "http-post"

    (* Component *)

    let private post s =
        Method.specification Name (set [ POST ]) (s,
            Content.specification Name (
                Existence.specification Name (
                    Responses.Moved.specification Name (
                        Responses.Missing.specification Name),
                    Preconditions.Common.specification Name (
                        Preconditions.Unsafe.specification Name (
                            Conflict.specification Name (
                                Operation.specification Name POST (
                                    Responses.Created.specification Name (
                                        Responses.Other.specification Name (
                                            Responses.Common.specification Name)))))))))

    let component =
        { Metadata =
            { Name = Name
              Description = None }
          Requirements =
            { Required = set [ Core.Name ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ Core.Name; "fallback"; "fallback-decision" ], Right, post) ] }
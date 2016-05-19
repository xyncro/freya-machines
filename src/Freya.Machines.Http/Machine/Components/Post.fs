namespace Freya.Machines.Http

#nowarn "46"

open Arachne.Http
open Hephaestus

(* Post *)

[<RequireQualifiedAccess>]
module internal Post =

    (* Name *)

    [<Literal>]
    let private Post =
        "post"

    (* Component *)

    let private post s =
        Method.specification Post (set [ POST ]) (
            s, Existence.specification Post (
                Responses.Moved.specification Post (
                    Responses.Missing.specification Post),
                Preconditions.Common.specification Post (
                    Preconditions.Unsafe.specification Post (
                        Conflict.specification Post (
                            Operations.specification Post POST (
                                Responses.Created.specification Post (
                                    Responses.Other.specification Post (
                                        Responses.Common.specification Post))))))))

    let component =
        { Metadata =
            { Name = "http.post"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ "http"; "end-decision" ], Right, post) ] }
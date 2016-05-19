namespace Freya.Machines.Http

#nowarn "46"

open Arachne.Http
open Hephaestus

(* Options *)

[<RequireQualifiedAccess>]
module internal Options =

    (* Name *)

    [<Literal>]
    let private Options =
        "options"

    (* Component *)

    let private options s =
        Method.specification Options (set [ OPTIONS ]) (
            s, Responses.Options.specification Options)

    let component =
        { Metadata =
            { Name = "http.options"
              Description = None }
          Requirements =
            { Required = set [ "http.core" ]
              Preconditions = List.empty }
          Operations =
            [ Splice (Key [ "http"; "core"; "validation"; "bad-request-decision" ], Left, options) ] }

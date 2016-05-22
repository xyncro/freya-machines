namespace Freya.Machines.Http.Cors

#nowarn "46"

open Freya.Core
open Freya.Machines
open Hephaestus

(* Preflight *)

[<RequireQualifiedAccess>]
module internal Preflight =

    (* Name *)

    [<Literal>]
    let private Preflight =
        "preflight"

    (* Component *)

    let component : Component<Configuration,unit,State> =
        { Metadata = 
            { Name = "http.cors.preflight"
              Description = None }
          Requirements =
            { Required = Set.empty
              Preconditions = List.empty }
          Operations =
            [] }

namespace Freya.Machines.Http

#nowarn "46"

open Hephaestus

(* Core *)

[<RequireQualifiedAccess>]
module internal Core =

    (* Name *)

    [<Literal>]
    let private Core =
        "core"

    (* Component *)

    let private core =
        Assertions.specification Core (
            Permissions.specification Core (
                Validations.specification Core (
                    Negotiations.specification Core (
                        Fallback.specification Core))))

    let component =
        { Metadata = 
            { Name = "http.core"
              Description = None }
          Requirements =
            { Required = Set.empty
              Preconditions = List.empty }
          Operations =
            [ Prepend (fun _ -> core) ] }

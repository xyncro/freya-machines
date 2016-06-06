namespace Freya.Machines.Http

#nowarn "46"

open Hephaestus

(* Core *)

[<RequireQualifiedAccess>]
module internal Core =

    (* Name *)

    [<Literal>]
    let Name =
        "http-core"

    (* Component *)

    let private core =
        Assertions.specification Name (
            Permissions.specification Name (
                Validations.specification Name (
                    Negotiations.specification Name (
                        Fallback.specification Name))))

    let component =
        { Metadata = 
            { Name = Name
              Description = None }
          Requirements =
            { Required = Set.empty
              Preconditions = List.empty }
          Operations =
            [ Prepend (fun _ -> core) ] }

namespace Freya.Machines.Http.Machine.Configuration

open Aether.Operators
open Freya.Core
open Freya.Machines
open Hephaestus

(* Extensions

   Extensions of the basic HTTP machine, defined as sets of Hephaestus
   components, which will be added to the default set of HTTP components when
   the machine is built and optmized at runtime. *)

[<RequireQualifiedAccess>]
module Extensions =

    (* Types *)

    type private Extensions =
        { Components: Set<Component<Configuration,unit,State>> }

        static member components_ =
            (fun x -> x.Components), (fun c x -> { x with Extensions.Components = c })

        static member empty =
            { Components = Set.empty }

    (* Optics *)

    let private extensions_ =
        Configuration.element_ Extensions.empty [ "http"; "configuration"; "extensions" ]

    (* Request *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Components =

        let components_ =
                extensions_
            >-> Extensions.components_
namespace Freya.Machines

open Aether
open Aether.Operators
open Freya.Core

(* Types *)

type Configuration =
    | Configuration of Map<string,obj>

    static member configuration_ =
        (fun (Configuration x) -> x), (Configuration)

    static member empty =
        Configuration (Map.empty)

(* Configuration

   Functions for working with the Configuration type, particularly using optics
   to drive access and modification, providing "safe" ways to access
   indeterminate properties of the system at runtime. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Configuration =

    (* Optics *)

    let private default_ def =
        (function | Some a -> a
                  | _ -> def), (Some)

    let element_<'a> def element =
            Lens.ofIsomorphism Configuration.configuration_
        >-> Map.value_ element
        >-> Option.mapIsomorphism box_<'a>
        >-> default_ def
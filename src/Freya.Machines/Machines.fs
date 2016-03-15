module Freya.Machines

open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Hephaestus

(* Types

   Types representing some core concepts within the design and configuration
   of a conceptual Machine for the Freya system.

   The concept of a Decision maps the core Hephaestus abstraction of a
   decision of left or right to Freya, using simple boolean true or false to
   represent branching points (left or right are always intended to be an
   abstraction which should be targeted by a domain specific abstraction.

   The concept of Configuration represents an extensible container for settings
   of any kind, which may be populated by any relevant component within the
   system to provide a source of setting information which cannot be known at
   design time.

   Optics are provided for these core Machine types. *)

type Value<'a> =
    | Dynamic of Freya<'a>
    | Static of 'a

    static member dynamic_ =
        (function | Dynamic f -> Some f
                  | _ -> None), (Dynamic)

    static member static_ =
        (function | Static l -> Some l
                  | _ -> None), (Static)

type Configuration =
    | Configuration of Map<string,obj>

    static member configuration_ =
        (fun (Configuration x) -> x), (Configuration)

    static member empty =
        Configuration (Map.empty)

(* Value

   Functions for working with Freya values, binding a Freya value (function)
   inside a Value, mapping the dynamic or static Value function to the Freya
   function. *)

[<RequireQualifiedAccess>]
module Value =

    [<RequireQualifiedAccess>]
    module Freya =

        let bind f =
            function | Dynamic x -> Dynamic (f =<< x)
                     | Static x -> Dynamic (f x)

(* Freya

   Functions for working with Values, applying them to Freya functions, and
   lifting the contained values to Freya functions where required (for both
   simple and optional values). *)

[<RequireQualifiedAccess>]
module Freya =

    [<RequireQualifiedAccess>]
    module Value =

        let apply f =
            function | Dynamic x -> f =<< x
                     | Static x -> f x

        let lift =
            function | Dynamic x -> x
                     | Static x -> Freya.init x

        let liftOption =
            function | Some v -> Some <!> lift v
                     | _ -> Freya.init None

(* Decisions

   Simple mapping from a Freya Decision to a stateful Hephaestus Decision,
   parameterized by the Freya State type. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Decision =

    let private convert =
        function | true -> Right
                 | _ -> Left

    let map =
        function | Dynamic f -> Function (convert <!> f) 
                 | Static l -> Literal (convert l)

(* Settings

   Functions for working with the Settings type, particularly using lenses to
   drive access and modification, providing "safe" ways to access indeterminate
   properties of the system at runtime. *)

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

(* Patterns

   Commonly useful active recognizers for working with lens based access to
   data structures, particularly useful here for making access to configuration
   more concise. *)

let inline (|Get|) optic =
    Optic.get optic

let inline (|TryGet|_|) optic =
    Optic.get optic
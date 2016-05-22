namespace Freya.Machines.Http

open Aether
open Aether.Operators
open Freya.Machines
open Hephaestus

(* Fallback

   Decision determining whether the requested operation would cause a
   conflict given the current state of the resource.

   Where a conflict would be caused, a 409 response is returned,
   signalling a client error. *)

[<RequireQualifiedAccess>]
module Fallback =

    (* Key *)

    let private key p =
        Key.add [ p; "fallback" ] Key.root

    (* Types *)

    type private Fallback =
        { Terminals: Terminals }

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Terminals = Terminals.empty }

     and private Terminals =
        { Fallback: Handler option }

        static member fallback_ =
            (fun x -> x.Fallback), (fun f x -> { x with Terminals.Fallback = f })

        static member empty =
            { Fallback = None }

    (* Optics *)

    let private fallback_ =
        Configuration.element_ Fallback.empty "http.specifications.fallback"

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                fallback_
            >-> Fallback.terminals_

        let fallback_ =
                terminals_
            >-> Terminals.fallback_

        let internal fallback p =
            Terminal.create (key p, "fallback")
                (function | _ -> Operation.ok None None)
                (function | Get fallback_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let internal fallback p =
            Decision.create (key p, "fallback")
                (function | _ -> Static true)
                (Specification.Terminal.empty, Terminals.fallback p)

    (* Specification *)

    let internal specification =
        Decisions.fallback
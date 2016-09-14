namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Machines
open Freya.Machines.Http
open Hephaestus

(* Fallback

   Decision determining whether the requested operation would cause a
   conflict given the current state of the resource.

   Where a conflict would be caused, a 409 response is returned,
   signalling a client error. *)

[<RequireQualifiedAccess>]
module Fallback =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "fallback" ]

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
        Configuration.element_ Fallback.empty [ "http"; "specifications"; "fallback" ]

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

        let fallback k =
            Terminal.create (key k, "handleFallback")
                (function | _ -> Operations.ok None None)
                (function | Get fallback_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let fallback k =
            Decision.create (key k, "fallback")
                (function | _ -> Static true)
                (Specification.Terminal.empty, Terminals.fallback k)

    (* Specification *)

    let specification =
        Decisions.fallback
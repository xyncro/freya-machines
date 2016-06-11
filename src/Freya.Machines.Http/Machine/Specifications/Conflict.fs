namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Machines
open Freya.Machines.Http

(* Conflict

   Decision determining whether the requested operation would cause a
   conflict given the current state of the resource.

   Where a conflict would be caused, a 409 response is returned,
   signalling a client error. *)

[<RequireQualifiedAccess>]
module Conflict =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "conflict" ]

    (* Types *)

    type private Conflict =
        { Decisions: Decisions
          Terminals: Terminals }

        static member decisions_ =
            (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Decisions = Decisions.empty
              Terminals = Terminals.empty }

     and private Decisions =
        { Conflict: Value<bool> option }

        static member conflict_ =
            (fun x -> x.Conflict), (fun e x -> { x with Decisions.Conflict = e })

        static member empty =
            { Conflict = None }

     and private Terminals =
        { Conflict: Handler option }

        static member conflict_ =
            (fun x -> x.Conflict), (fun e x -> { x with Terminals.Conflict = e })

        static member empty =
            { Conflict = None }

    (* Optics *)

    let private conflict_ =
        Configuration.element_ Conflict.empty [ "http"; "specifications"; "conflict" ]

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                conflict_
            >-> Conflict.terminals_

        let conflict_ =
                terminals_
            >-> Terminals.conflict_

        let conflict p =
            Terminal.create (key p, "conflict")
                (function | _ -> Operations.conflict)
                (function | Get conflict_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                conflict_
            >-> Conflict.decisions_

        let conflict_ =
                decisions_
            >-> Decisions.conflict_

        let conflict p s =
            Decision.create (key p, "conflict")
                (function | TryGetOrElse conflict_ (Static false) x -> x)
                (s, Terminals.conflict p)

    (* Specification *)

    let specification =
        Decisions.conflict
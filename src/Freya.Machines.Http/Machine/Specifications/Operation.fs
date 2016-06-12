namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core
open Freya.Machines
open Freya.Machines.Http

(* Operation

   Decisions representing the processing of a specific operation (one
   of the non-idempotent methods supported by the HTTP model). The
   decision for the operation is a Freya<bool> and thus will always
   remain dynamic throughout optimization (if present). *)

[<RequireQualifiedAccess>]
module Operation =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "operation" ]

    (* Types *)

    type private Operations =
        { Operations: Map<Method,Freya<bool>>
          Decisions: Decisions
          Terminals: Terminals }

        static member operations_ =
            (fun x -> x.Operations), (fun o x -> { x with Operations.Operations = o })

        static member decisions_ =
            (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Operations = Map.empty
              Decisions = Decisions.empty
              Terminals = Terminals.empty }

     and private Decisions =
        { Completed: Value<bool> option }

        static member completed_ =
            (fun x -> x.Completed), (fun c x -> { x with Completed = c })

        static member empty =
            { Completed = None }

     and private Terminals =
        { InternalServerError: Handler option
          Accepted: Handler option }

        static member internalServerError_ =
            (fun x -> x.InternalServerError), (fun i x -> { x with InternalServerError = i })

        static member accepted_ =
            (fun x -> x.Accepted), (fun a x -> { x with Accepted = a })

        static member empty =
            { InternalServerError = None
              Accepted = None }

    (* Optics *)

    let private operations_ =
        Configuration.element_ Operations.empty [ "http"; "specifications"; "operation" ]

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                operations_
            >-> Operations.terminals_

        let internalServerError_ =
                terminals_
            >-> Terminals.internalServerError_

        let accepted_ =
                terminals_
            >-> Terminals.accepted_

        let internalServerError k =
            Terminal.create (key k, "internal-server-error")
                (function | _ -> Operations.internalServerError)
                (function | Get internalServerError_ x -> x)

        let accepted k =
            Terminal.create (key k, "accepted")
                (function | _ -> Operations.accepted)
                (function | Get accepted_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                operations_
            >-> Operations.decisions_

        let completed_ =
                decisions_
            >-> Decisions.completed_

        let operationMethod_ m =
                operations_
            >-> Operations.operations_
            >-> Map.value_ m

        let rec operation k m s =
            Decision.create (key k, "operation")
                (function | TryGet (operationMethod_ m) f -> Dynamic (f)
                          | _ -> Static true)
                (Terminals.internalServerError k, completed k s)

        and completed k s =
            Decision.create (key k, "completed")
                (function | TryGet completed_ x -> x
                          | _ -> Static true)
                (Terminals.accepted k, s)

    (* Specification *)

    let specification =
        Decisions.operation
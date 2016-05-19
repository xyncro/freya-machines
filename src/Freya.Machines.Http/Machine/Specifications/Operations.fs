namespace Freya.Machines.Http

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core
open Freya.Machines

(* Operation

   Decisions representing the processing of a specific operation (one
   of the non-idempotent methods supported by the HTTP model). The
   decision for the operation is a Freya<bool> and thus will always
   remain dynamic throughout optimization (if present). *)

[<RequireQualifiedAccess>]
module Operations =

    (* Key *)

    let private key p =
        Key.add [ p; "operation" ] Key.root

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
        Configuration.element_ Operations.empty "http.specifications.operations"

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

        let internal internalServerError p =
            Terminal.create (key p, "internal-server-error")
                (function | _ -> Operation.internalServerError)
                (function | Get internalServerError_ x -> x)

        let internal accepted p =
            Terminal.create (key p, "accepted")
                (function | _ -> Operation.accepted)
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

        let rec internal operation p m s =
            Decision.create (key p, "operation")
                (function | TryGet (operationMethod_ m) f -> Dynamic (f)
                          | _ -> Static true)
                (Terminals.internalServerError p, completed p s)

        and internal completed p s =
            Decision.create (key p, "completed")
                (function | TryGet completed_ x -> x
                          | _ -> Static true)
                (Terminals.accepted p, s)

    (* Specification *)

    let internal specification =
        Decisions.operation
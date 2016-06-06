namespace Freya.Machines.Http

open Aether.Operators
open Freya.Machines

(* Existence

   Decision determining whether or not the relevant resource exists at
   this point (it may have existed previously, but this is about the
   existence of the resource currently).

   The element does not result in a response, only in control flow of
   the machine, and as such must be provided with both left and right
   cases (no terminals are implied). *)

[<RequireQualifiedAccess>]
module Existence =

    let private key =
        Key.root >> Key.add [ "existence" ]

    (* Types *)

    type private Existence =
        { Decisions: Decisions }

        static member decisions_ =
            (fun x -> x.Decisions), (fun d x -> { x with Decisions = d })

        static member empty =
            { Decisions = Decisions.empty }

     and private Decisions =
        { Exists: Value<bool> option }

        static member exists_ =
            (fun x -> x.Exists), (fun e x -> { x with Exists = e })

        static member empty =
            { Exists = None }

    (* Optics *)

    let private existence_ =
        Configuration.element_ Existence.empty [ "http"; "specifications"; "existence" ]

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                existence_
            >-> Existence.decisions_

        let exists_ =
                decisions_
            >-> Decisions.exists_

        let internal exists p =
            Decision.create (key p, "exists")
                (function | TryGet exists_ x -> x
                          | _ -> Static true)

    (* Specification *)

    let internal specification =
        Decisions.exists
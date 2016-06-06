namespace Freya.Machines.Http

open Aether
open Aether.Operators
open Freya.Machines

(* Permissions

   Decisions determining the permission of the client to make the
   current request, whether for reasons of authorization or allowance.

   Failures of these checks will result in  401 or 403 responses,
   signalling a client error. *)

[<RequireQualifiedAccess>]
module Permissions =

    (* Types *)

    type private Permissions =
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
        { Authorized: Value<bool> option
          Allowed: Value<bool> option }

        static member authorized_ =
            (fun x -> x.Authorized), (fun a x -> { x with Authorized = a })

        static member allowed_ =
            (fun x -> x.Allowed), (fun a x -> { x with Allowed = a })

        static member empty =
            { Authorized = None
              Allowed = None }

     and private Terminals =
        { Unauthorized: Handler option
          Forbidden: Handler option }

        static member unauthorized_ =
            (fun x -> x.Unauthorized), (fun u x -> { x with Unauthorized = u })

        static member forbidden_ =
            (fun x -> x.Forbidden), (fun u x -> { x with Forbidden = u })

        static member empty =
            { Unauthorized = None
              Forbidden = None }

    (* Key *)

    let private key =
        Key.root >> Key.add [ "permissions" ]

    (* Optics *)

    let private permissions_ =
        Configuration.element_ Permissions.empty [ "http"; "specifications"; "permissions" ]

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                permissions_
            >-> Permissions.terminals_

        let unauthorized_ =
                terminals_
            >-> Terminals.unauthorized_

        let forbidden_ =
                terminals_
            >-> Terminals.forbidden_

        let internal unauthorized p =
            Terminal.create (key p, "unauthorized")
                (function | _ -> Operation.unauthorized)
                (function | Get unauthorized_ x -> x)

        let internal forbidden p =
            Terminal.create (key p, "forbidden")
                (function | _ -> Operation.forbidden)
                (function | Get forbidden_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                permissions_
            >-> Permissions.decisions_

        let authorized_ =
                decisions_
            >-> Decisions.authorized_

        let allowed_ =
                decisions_
            >-> Decisions.allowed_

        let rec internal authorized p s =
            Decision.create (key p, "authorized")
                (function | TryGet authorized_ x -> x
                          | _ -> Static true)
                (Terminals.unauthorized p, allowed p s)

        and internal allowed p s =
            Decision.create (key p, "allowed")
                (function | TryGet allowed_ x -> x
                          | _ -> Static true)
                (Terminals.forbidden p, s)

    (* Specification *)

    let internal specification =
        Decisions.authorized

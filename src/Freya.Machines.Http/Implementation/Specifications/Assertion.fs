namespace Freya.Machines.Http.Implementation.Specifications

open Aether
open Aether.Operators
open Freya.Machines
open Freya.Machines.Http.Semantics

(* Types *)

type private Assertion =
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
    { ServiceAvailable: Value<bool> option
      HttpVersionSupported: Value<bool> option }

    static member serviceAvailable_ =
        (fun x -> x.ServiceAvailable), (fun s x -> { x with ServiceAvailable = s })

    static member httpVersionSupported_ =
        (fun x -> x.HttpVersionSupported), (fun h x -> { x with HttpVersionSupported = h })

    static member empty =
        { ServiceAvailable = None
          HttpVersionSupported = None }

 and private Terminals =
    { ServiceUnavailable: Handler option
      HttpVersionNotSupported: Handler option
      NotImplemented: Handler option }

    static member serviceUnavailable_ =
        (fun x -> x.ServiceUnavailable), (fun s x -> { x with ServiceUnavailable = s })

    static member httpVersionNotSupported_ =
        (fun x -> x.HttpVersionNotSupported), (fun h x -> { x with HttpVersionNotSupported = h })

    static member notImplemented_ =
        (fun x -> x.NotImplemented), (fun n x -> { x with NotImplemented = n })

    static member empty =
        { ServiceUnavailable = None
          HttpVersionNotSupported = None
          NotImplemented = None }

(* Assertion *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Assertion =

    (* Key *)

    let private key p =
        Key.add [ p; "assertion" ] Key.root

    (* Assertion *)

    let private root_ =
        Configuration.element_ Assertion.empty "http.specifications.assertion"

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                root_
            >-> Assertion.terminals_

        let serviceUnavailable_ =
                terminals_
            >-> Terminals.serviceUnavailable_

        let httpVersionNotSupported_ =
                terminals_
            >-> Terminals.httpVersionNotSupported_

        let notImplemented_ =
                terminals_
            >-> Terminals.notImplemented_

        let serviceUnavailable p =
            Terminal.create (key p, "service-unavailable")
                (Optic.get serviceUnavailable_) Operation.serviceUnavailable

        let httpVersionNotSupported p =
            Terminal.create (key p, "http-version-not-supported")
                (Optic.get httpVersionNotSupported_) Operation.httpVersionNotSupported

        let notImplemented p =
            Terminal.create (key p, "not-implemented")
                (Optic.get notImplemented_) Operation.notImplemented
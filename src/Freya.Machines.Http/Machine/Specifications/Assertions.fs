namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Semantics
open Freya.Optics.Http

(* Assertions

   Decisions asserting the capability of the server to handle the given
   request, based on the defined availability, HTTP protocols, etc.

   Failures of these assertions/checks of server capability will result
   in 5xx error responses, signalling a server error of some type. *)

[<RequireQualifiedAccess>]
module internal Assertions =

    (* Types *)

    type private Assertions =
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

    (* Key *)

    let private key p =
        Key.add [ p; "assertion" ] Key.root

    (* Optics *)

    let private assertions_ =
        Configuration.element_ Assertions.empty "http.specifications.assertions"

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                assertions_
            >-> Assertions.terminals_

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
                (function | _ -> Operation.serviceUnavailable)
                (function | Get serviceUnavailable_ x -> x) 

        let httpVersionNotSupported p =
            Terminal.create (key p, "http-version-not-supported")
                (function | _ -> Operation.httpVersionNotSupported)
                (function | Get httpVersionNotSupported_ x -> x)

        let notImplemented p =
            Terminal.create (key p, "not-implemented")
                (function | _ -> Operation.notImplemented)
                (function | Get notImplemented_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                assertions_
            >-> Assertions.decisions_

        let serviceAvailable_ =
                decisions_
            >-> Decisions.serviceAvailable_

        let httpVersionSupported_ =
                decisions_
            >-> Decisions.httpVersionSupported_

        let rec serviceAvailable p s =
            Decision.create (key p, "service-available")
                (function | TryGet serviceAvailable_ x -> x
                          | _ -> Static true)
                (Terminals.serviceUnavailable p, httpVersionSupported p s)

        and httpVersionSupported p s =
            Decision.create (key p, "http-version-supported")
                (function | TryGet httpVersionSupported_ x -> x
                          | _ -> Dynamic supported)
                (Terminals.httpVersionNotSupported p, methodImplemented p s)

        and private supported =
                function | HTTP x when x >= 1.1 -> true
                         | _ -> false
            <!> !. Request.httpVersion_

        and methodImplemented p s =
            Decision.create (key p, "method-implemented")
                (function | TryGet Properties.Request.methods_ x -> bind knownCustom x
                          | _ -> Dynamic nonCustom)
                (Terminals.notImplemented p, s)

        and private knownCustom methodsAllowed =
                function | Method.Custom x when not (Set.contains (Method.Custom x) methodsAllowed) -> false
                         | _ -> true
            <!> !. Request.method_

        and private nonCustom =
                function | Method.Custom _ -> false
                         | _ -> true
            <!> !. Request.method_

    (* Specification *)

    let specification =
        Decisions.serviceAvailable
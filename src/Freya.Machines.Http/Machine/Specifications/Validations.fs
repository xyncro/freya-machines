namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http
open Freya.Machines.Http.Machine.Configuration
open Freya.Optics.Http

(* Validation

   Decisions determing the basic syntactic and semantic validity of the
   request made by the client, such as whether the method requested is
   allowed, whether the URI is of an appropriate length, etc.

   Failures of these checks will result in 4xx responses of the
   appropriate value, signalling a client error of some type. *)

(* Validation *)

[<RequireQualifiedAccess>]
module Validations =

    (* Types *)

    type private Validations =
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
        { ExpectationMet: Value<bool> option
          UriTooLong: Value<bool> option
          BadRequest: Value<bool> option }

        static member expectationMet_ =
            (fun x -> x.ExpectationMet), (fun e x -> { x with ExpectationMet = e })

        static member uriTooLong_ =
            (fun x -> x.UriTooLong), (fun u x -> { x with Decisions.UriTooLong = u })

        static member badRequest_ =
            (fun x -> x.BadRequest), (fun b x -> { x with Decisions.BadRequest = b })

        static member empty =
            { ExpectationMet = None
              UriTooLong = None
              BadRequest = None }

     and private Terminals =
        { ExpectationFailed: Handler option
          MethodNotAllowed: Handler option
          UriTooLong: Handler option
          BadRequest: Handler option }

        static member expectationFailed_ =
            (fun x -> x.ExpectationFailed), (fun e x -> { x with ExpectationFailed = e })

        static member methodNotAllowed_ =
            (fun x -> x.MethodNotAllowed), (fun e x -> { x with MethodNotAllowed = e })

        static member uriTooLong_ =
            (fun x -> x.UriTooLong), (fun u x -> { x with Terminals.UriTooLong = u })

        static member badRequest_ =
            (fun x -> x.BadRequest), (fun b x -> { x with Terminals.BadRequest = b })

        static member empty =
            { ExpectationFailed = None
              MethodNotAllowed = None
              UriTooLong = None
              BadRequest = None }

    (* Key *)

    let private key =
        Key.root >> Key.add [ "validations" ]

    (* Optics *)

    let private validations_ =
        Configuration.element_ Validations.empty [ "http"; "specifications"; "validations" ]

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                validations_
            >-> Validations.terminals_

        let expectationFailed_ =
                terminals_
            >-> Terminals.expectationFailed_

        let methodNotAllowed_ =
                terminals_
            >-> Terminals.methodNotAllowed_

        let uriTooLong_ =
                terminals_
            >-> Terminals.uriTooLong_

        let badRequest_ =
                terminals_
            >-> Terminals.badRequest_

        let expectationFailed k =
            Terminal.create (key k, "expectation-failed")
                (function | _ -> Operations.expectationFailed)
                (function | Get expectationFailed_ x -> x)

        let methodNotAllowed k =
            Terminal.create (key k, "method-not-allowed")
                (function | TryGet Properties.Request.methods_ x -> Freya.Value.apply Operations.methodNotAllowed x
                          | _ -> Operations.methodNotAllowed Defaults.methods)
                (function | Get methodNotAllowed_ x -> x)

        let uriTooLong k =
            Terminal.create (key k, "uri-too-long")
                (function | _ -> Operations.uriTooLong)
                (function | Get uriTooLong_ x -> x)

        let badRequest k =
            Terminal.create (key k, "bad-request")
                (function | _ -> Operations.badRequest)
                (function | Get badRequest_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let private decisions_ =
                validations_
            >-> Validations.decisions_

        let expectationMet_ =
                decisions_
            >-> Decisions.expectationMet_

        let uriTooLong_ =
                decisions_
            >-> Decisions.uriTooLong_

        let badRequest_ =
                decisions_
            >-> Decisions.badRequest_

        let rec expectationMet k s =
            Decision.create (key k, "expectation-met")
                (function | TryGetOrElse expectationMet_ (Static true) x -> x)
                (Terminals.expectationFailed k, methodAllowed k s)

        and methodAllowed k s =
            Decision.create (key k, "method-allowed")
                (function | TryGet Properties.Request.methods_ x -> Value.Freya.bind allowed x
                          | _ -> Dynamic (allowed Defaults.methods))
                (Terminals.methodNotAllowed k, uriTooLong k s)

        and private allowed s =
                function | x when Set.contains x s -> true
                         | _ -> false
            <!> !. Request.method_

        and uriTooLong k s =
            Decision.create (key k, "uri-too-long")
                (function | TryGetOrElse uriTooLong_ (Static false) x -> x)
                (badRequest k s, Terminals.uriTooLong k)

        and badRequest k s =
            Decision.create (key k, "bad-request")
                (function | TryGetOrElse badRequest_ (Static false) x -> x)
                (s, Terminals.badRequest k)

    (* Specification *)

    let specification =
        Decisions.expectationMet
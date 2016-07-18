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
module Content =

    (* Types *)

    type private Content =
        { Terminals: Terminals }

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Terminals = Terminals.empty }

     and private Terminals =
        { LengthRequired: Handler option
          UnsupportedMediaType: Handler option }

        static member lengthRequired_ =
            (fun x -> x.LengthRequired), (fun l x -> { x with LengthRequired = l })

        static member unsupportedMediaType_ =
            (fun x -> x.UnsupportedMediaType), (fun u x -> { x with UnsupportedMediaType = u })

        static member empty =
            { LengthRequired = None
              UnsupportedMediaType = None }

    (* Key *)

    let private key =
        Key.root >> Key.add [ "content" ]

    (* Optics *)

    let private content_ =
        Configuration.element_ Content.empty [ "http"; "specifications"; "content" ]

    (* Terminals *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Terminals =

        let private terminals_ =
                content_
            >-> Content.terminals_

        let lengthRequired_ =
                terminals_
            >-> Terminals.lengthRequired_

        let unsupportedMediaType_ =
                terminals_
            >-> Terminals.unsupportedMediaType_

        let lengthRequired k =
            Terminal.create (key k, "length-required")
                (function | _ -> Operations.lengthRequired)
                (function | Get lengthRequired_ x -> x)

        let unsupportedMediaType k =
            Terminal.create (key k, "unsupported-media-type")
                (function | _ -> Operations.unsupportedMediaType)
                (function | Get unsupportedMediaType_ x -> x)

    (* Decisions *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Decisions =

        let rec lengthDefined k s =
            Decision.create (key k, "length-defined")
                (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.contentLength_))
                (Terminals.lengthRequired k, mediaTypeSupported k s)

        // TODO: Media Type Logic

        and mediaTypeSupported k s =
            Decision.create (key k, "media-type-supported")
                (function | _ -> Static true)
                (Terminals.unsupportedMediaType k, s)

    (* Specification *)

    let specification =
        Decisions.lengthDefined
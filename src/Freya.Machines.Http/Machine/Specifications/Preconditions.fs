namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http
open Freya.Machines.Http.Machine.Configuration
open Freya.Optics.Http
open Freya.Types.Http

(* Preconditions

   Decisions representing the negotiation of optional preconditions
   declared by the client, which should preclude further processing of
   the request if not met. Preconditions are divided in to common
   (applying to all requests) and safe/unsafe, where the unsafe set
   are applicable to methods which would change state.

   Preconditions failing results in a 412 response. *)

[<RequireQualifiedAccess>]
module Preconditions =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "preconditions" ]

    (* Types *)

    type private Preconditions =
        { Terminals: Terminals }

        static member terminals_ =
            (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

        static member empty =
            { Terminals = Terminals.empty }

     and Terminals =
        { PreconditionFailed: Handler option }

        static member preconditionFailed_ =
            (fun x -> x.PreconditionFailed), (fun p x -> { x with PreconditionFailed = p })

        static member empty =
            { PreconditionFailed = None }

    (* Optics *)

    let private preconditions_ =
        Configuration.element_ Preconditions.empty [ "http"; "specifications"; "preconditions" ]

    (* Shared *)

    [<RequireQualifiedAccess>]
    module Shared =

        (* Terminals *)

        [<RequireQualifiedAccess>]
        module Terminals =

            let private terminals_ =
                    preconditions_
                >-> Preconditions.terminals_

            let preconditionFailed_ =
                    terminals_
                >-> Terminals.preconditionFailed_

            let preconditionFailed k =
                Terminal.create (key k, "handlePreconditionFailed")
                    (function | _ -> Operations.preconditionFailed)
                    (function | Get preconditionFailed_ x -> x)

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key k =
            Key.add [ "common" ] (key k)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec hasIfMatch k s =
                Decision.create (key k, "hasIfMatch")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifMatch_))
                    (hasIfUnmodifiedSince k s, ifMatchMatches k s)

            and ifMatchMatches k s =
                Decision.create (key k, "ifMatchMatches")
                    (function | TryGet Properties.Resource.entityTag_ x -> Value.Freya.bind matches x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed k, s)

            and private matches entityTag =
                    function | Some (IfMatch (IfMatchChoice.EntityTags x)) when exists entityTag x -> true
                             | Some (IfMatch (IfMatchChoice.Any)) -> true
                             | _ -> false
                <!> !. Request.Headers.ifMatch_

            and private exists =
                    function | Strong x -> List.exists (strong x)
                             | Weak x -> List.exists (weak x)

            and private strong x =
                    function | Strong y when x = y -> true
                             | _ -> false
            and private weak x = 
                    function | Weak y when x = y -> true
                             | _ -> false

            and hasIfUnmodifiedSince k s =
                Decision.create (key k, "hasIfUnmodifiedSince")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifUnmodifiedSince_))
                    (s, ifUnmodifiedSinceMatches k s)

            and ifUnmodifiedSinceMatches k s =
                Decision.create (key k, "ifUnmodifiedSinceMatches")
                    (function | TryGet Properties.Resource.lastModified_ x -> Value.Freya.bind earlier x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed k, s)

            and private earlier date =
                    function | Some (IfUnmodifiedSince x) when date <= x -> true
                             | _ -> false
                <!> !. Request.Headers.ifUnmodifiedSince_

        (* Specification *)

        let specification =
            Decisions.hasIfMatch

    (* Safe *)

    [<RequireQualifiedAccess>]
    module Safe =

        (* Key *)

        let private key k =
            Key.add [ "safe" ] (key k)

        (* Types *)

        type private Safe =
            { Terminals: Terminals }

            static member terminals_ =
                (fun x -> x.Terminals), (fun t x -> { x with Safe.Terminals = t })

            static member empty =
                { Terminals = Terminals.empty }

         and Terminals =
            { NotModified: Handler option }

            static member notModified_ =
                (fun x -> x.NotModified), (fun n x -> { x with NotModified = n })

            static member empty =
                { NotModified = None }

        (* Optics *)

        let private safe_ =
            Configuration.element_ Safe.empty [ "http"; "specifications"; "preconditions"; "safe" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    safe_
                >-> Safe.terminals_

            let notModified_ =
                    terminals_
                >-> Terminals.notModified_

            let notModified k =
                Terminal.create (key k, "handleNotMdified")
                    (function | _ -> Operations.notModified)
                    (function | Get notModified_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec hasIfNoneMatch k s =
                Decision.create (key k, "hasIfNoneMatch")
                    (fun _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifNoneMatch_))
                    (hasIfModifiedSince k s, ifNoneMatchMatches k s)

            and ifNoneMatchMatches k s =
                Decision.create (key k, "ifNoneMatchMatches")
                    (function | TryGet Properties.Resource.entityTag_ x -> Value.Freya.bind matches x
                              | _ -> Static true)
                    (Terminals.notModified k, s)

            and private matches entityTag =
                    function | Some (IfNoneMatch (IfNoneMatchChoice.EntityTags x)) when not (exists entityTag x) -> true
                             | Some (IfNoneMatch (IfNoneMatchChoice.Any)) -> true
                             | _ -> false
                <!> !. Request.Headers.ifNoneMatch_

            and private exists =
                    function | Strong x 
                             | Weak x -> List.exists (weak x)

            and private weak x =
                    function | Strong y 
                             | Weak y -> x = y 

            and hasIfModifiedSince k s =
                Decision.create (key k, "hasIfModifiedSince")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifModifiedSince_))
                    (s, ifModifiedSinceMatches k s)

            and ifModifiedSinceMatches k s =
                Decision.create (key k, "ifModifiedSinceMatches")
                    (function | TryGet Properties.Resource.lastModified_ x -> Value.Freya.bind later x
                              | _ -> Static true)
                    (Terminals.notModified k, s)

            and private later date =
                    function | Some (IfModifiedSince x) when date > x -> true
                             | _ -> false
                <!> !. Request.Headers.ifModifiedSince_

        (* Specification *)

        let specification =
            Decisions.hasIfNoneMatch

    (* Unsafe *)

    [<RequireQualifiedAccess>]
    module Unsafe =

        (* Key *)

        let private key k =
            Key.add [ "unsafe" ] (key k)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec hasIfNoneMatch k s =
                Decision.create (key k, "hasIfNoneMatch")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifNoneMatch_))
                    (s, ifNoneMatchMatches k s)

            and ifNoneMatchMatches k s =
                Decision.create (key k, "ifNoneMatchMatches")
                    (function | TryGet Properties.Resource.entityTag_ x -> Value.Freya.bind matches x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed k, s)

            and private matches entityTag =
                    function | Some (IfNoneMatch (IfNoneMatchChoice.EntityTags x)) when not (exists entityTag x) -> true
                             | Some (IfNoneMatch (IfNoneMatchChoice.Any)) -> true
                             | _ -> false
                <!> !. Request.Headers.ifNoneMatch_

            and private exists =
                    function | Strong x 
                             | Weak x -> List.exists (weak x)

            and private weak x =
                    function | Strong y 
                             | Weak y -> x = y 

        (* Specification *)

        let specification =
            Decisions.hasIfNoneMatch
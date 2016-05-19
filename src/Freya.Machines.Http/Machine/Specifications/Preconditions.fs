namespace Freya.Machines.Http

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core.Operators
open Freya.Machines
open Freya.Optics.Http

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

    let private key p =
        Key.add [ p; "preconditions" ] Key.root

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
        Configuration.element_ Preconditions.empty "http.specifications.preconditions"

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

            let internal preconditionFailed p =
                Terminal.create (key p, "precondition-failed")
                    (function | _ -> Operation.preconditionFailed)
                    (function | Get preconditionFailed_ x -> x)

    (* Common *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key p =
            Key.add [ "common" ] (key p)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec internal hasIfMatch p s =
                Decision.create (key p, "has-if-match")
                    (fun _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifMatch_))
                    (hasIfUnmodifiedSince p s, ifMatchMatches p s)

            and internal ifMatchMatches p s =
                Decision.create (key p, "if-match-matches")
                    (function | TryGet Properties.Resource.entityTag_ x -> bind matches x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed p, s)

            and private matches entityTag =
                    function | Some (IfMatch (IfMatchChoice.EntityTags x)) when exists entityTag x -> true
                             | Some (IfMatch (IfMatchChoice.Any)) -> true
                             | _ -> false
                <!> !. Request.Headers.ifMatch_

            and private exists =
                    function | Strong x -> List.exists (strong x)
                             | _ -> fun _ -> false

            and private strong x =
                    function | Strong y when x = y -> true
                             | _ -> false

            and internal hasIfUnmodifiedSince p s =
                Decision.create (key p, "has-if-unmodified-since")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifUnmodifiedSince_))
                    (s, ifUnmodifiedSinceMatches p s)

            and internal ifUnmodifiedSinceMatches p s =
                Decision.create (key p, "if-unmodified-since-matches")
                    (function | TryGet Properties.Resource.lastModified_ x -> bind earlier x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed p, s)

            and private earlier date =
                    function | Some (IfUnmodifiedSince x) when date <= x -> true
                             | _ -> false
                <!> !. Request.Headers.ifUnmodifiedSince_

        (* Specification *)

        let internal specification =
            Decisions.hasIfMatch

    (* Safe *)

    [<RequireQualifiedAccess>]
    module Safe =

        (* Key *)

        let private key p =
            Key.add [ "safe" ] (key p)

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
            Configuration.element_ Safe.empty "http.specifications.preconditions.safe"

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

            let internal notModified p =
                Terminal.create (key p, "not-modified")
                    (function | _ -> Operation.notModified)
                    (function | Get notModified_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec internal hasIfNoneMatch p s =
                Decision.create (key p, "has-if-none-match")
                    (fun _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifNoneMatch_))
                    (hasIfModifiedSince p s, ifNoneMatchMatches p s)

            and internal ifNoneMatchMatches p s =
                Decision.create (key p, "if-none-match-matches")
                    (function | TryGet Properties.Resource.entityTag_ x -> bind matches x
                              | _ -> Static true)
                    (Terminals.notModified p, s)

            and private matches entityTag =
                    function | Some (IfNoneMatch (EntityTags x)) when not (exists entityTag x) -> true
                             | Some (IfNoneMatch (IfNoneMatchChoice.Any)) -> true
                             | _ -> false
                <!> !. Request.Headers.ifNoneMatch_

            and private exists =
                    function | Strong x 
                             | Weak x -> List.exists (weak x)

            and private weak x =
                    function | Strong y 
                             | Weak y -> x = y 

            and internal hasIfModifiedSince p s =
                Decision.create (key p, "has-if-modified-since")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifModifiedSince_))
                    (s, ifModifiedSinceMatches p s)

            and internal ifModifiedSinceMatches p s =
                Decision.create (key p, "if-modified-since-matches")
                    (function | TryGet Properties.Resource.lastModified_ x -> bind later x
                              | _ -> Static true)
                    (Terminals.notModified p, s)

            and private later date =
                    function | Some (IfModifiedSince x) when date > x -> true
                             | _ -> false
                <!> !. Request.Headers.ifModifiedSince_

        (* Specification *)

        let internal specification =
            Decisions.hasIfNoneMatch

    (* Unsafe *)

    [<RequireQualifiedAccess>]
    module Unsafe =

        (* Key *)

        let private key p =
            Key.add [ "unsafe" ] (key p)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let rec internal hasIfNoneMatch p s =
                Decision.create (key p, "has-if-none-match")
                    (function | _ -> Dynamic (Option.isSome <!> !. Request.Headers.ifNoneMatch_))
                    (s, ifNoneMatchMatches p s)

            and internal ifNoneMatchMatches p s =
                Decision.create (key p, "if-none-match-matches")
                    (function | TryGet Properties.Resource.entityTag_ x -> bind matches x
                              | _ -> Static true)
                    (Shared.Terminals.preconditionFailed p, s)

            and private matches entityTag =
                    function | Some (IfNoneMatch (EntityTags x)) when not (exists entityTag x) -> true
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

        let internal specification =
            Decisions.hasIfNoneMatch
namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http
open Freya.Machines.Http.Machine.Configuration

(* Responses

   Decisions and terminals used for common response cases, decomposed
   to appropriate granularities. *)

[<RequireQualifiedAccess>]
module Responses =

    (* Key *)

    let private key =
        Key.root >> Key.add [ "responses" ]

    (* Common

       Common conclusions of successful requests, handling the case of
       an OK situation, or a (successful) no content situation. *)

    [<RequireQualifiedAccess>]
    module Common =

        (* Key *)

        let private key k =
            Key.add [ "common" ] (key k)

        (* Types *)

        type private Common =
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
            { NoContent: Value<bool> option }

            static member noContent_ =
                (fun x -> x.NoContent), (fun n x -> { x with Decisions.NoContent = n })

            static member empty =
                { NoContent = None }

         and private Terminals =
            { NoContent: Handler option
              Ok: Handler option }

            static member noContent_ =
                (fun x -> x.NoContent), (fun n x -> { x with Terminals.NoContent = n })

            static member ok_ =
                (fun x -> x.Ok), (fun o x -> { x with Ok = o })

            static member empty =
                { NoContent = None
                  Ok = None }

        (* Optics *)

        let private common_ =
            Configuration.element_ Common.empty [ "http"; "specifications"; "responses"; "common" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    common_
                >-> Common.terminals_

            let noContent_ =
                    terminals_
                >-> Terminals.noContent_

            let ok_ =
                    terminals_
                >-> Terminals.ok_

            let noContent k =
                Terminal.create (key k, "no-content")
                    (function | _ -> Operations.noContent)
                    (function | Get noContent_ x -> x)

            let rec ok k =
                Terminal.create (key k, "ok")
                    (function |   Get Properties.Resource.entityTag_ entityTag
                                & Get Properties.Resource.lastModified_ lastModified ->
                                        Freya.Value.liftOption entityTag
                                    >>= fun entityTag ->
                                        Freya.Value.liftOption lastModified
                                    >>= fun lastModified ->
                                        Operations.ok entityTag lastModified)
                    (function | Get ok_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let private decisions_ =
                    common_
                >-> Common.decisions_

            let noContent_ =
                    decisions_
                >-> Decisions.noContent_

            let noContent k =
                Decision.create (key k, "no-content")
                    (function | TryGetOrElse noContent_ (Static false) x -> x)
                    (Terminals.ok k, Terminals.noContent k)

        (* Specification *)

        let specification =
            Decisions.noContent

    (* Created

       A simple element encapsulating the creation terminal, with no
       decision-making requirement. *)

    [<RequireQualifiedAccess>]
    module Created =

        (* Key *)

        let private key k =
            Key.add [ "created" ] (key k)

        (* Types *)

        type private Created =
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
            { Created: Value<bool> option }

            static member created_ =
                (fun x -> x.Created), (fun e x -> { x with Decisions.Created = e })

            static member empty =
                { Created = None }

         and private Terminals =
            { Created: Handler option }

            static member created_ =
                (fun x -> x.Created), (fun e x -> { x with Terminals.Created = e })

            static member empty =
                { Created = None }

        (* Optics *)

        let private created_ =
            Configuration.element_ Created.empty [ "http"; "specifications"; "responses"; "created" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    created_
                >-> Created.terminals_

            let created_ =
                    terminals_
                >-> Terminals.created_

            let created k =
                Terminal.create (key k, "created")
                    (function | _ -> Operations.created)
                    (function | Get created_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let private decisions_ =
                    created_
                >-> Created.decisions_

            let created_ =
                    decisions_
                >-> Decisions.created_

            let created k s =
                Decision.create (key k, "created")
                    (function | TryGetOrElse created_ (Static false) x -> x)
                    (s, Terminals.created k)

        (* Specification *)

        let internal specification =
            Decisions.created

    (* Missing *)

    [<RequireQualifiedAccess>]
    module Missing =

        (* Key *)

        let private key k =
            Key.add [ "missing" ] (key k)

        (* Types *)

        type private Missing =
            { Terminals: Terminals }

            static member terminals_ =
                (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

            static member empty =
                { Terminals = Terminals.empty }

         and private Terminals =
            { NotFound: Handler option }

            static member notFound_ =
                (fun x -> x.NotFound), (fun n x -> { x with NotFound = n })

            static member empty =
                { NotFound = None }

        (* Optics *)

        let private missing_ =
            Configuration.element_ Missing.empty [ "http"; "specifications"; "responses"; "missing" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    missing_
                >-> Missing.terminals_

            let notFound_ =
                    terminals_
                >-> Terminals.notFound_

            let notFound k =
                Terminal.create (key k, "not-found")
                    (function | _ -> Operations.notFound)
                    (function | Get notFound_ x -> x)

        (* Specification *)

        let specification =
            Terminals.notFound

    (* Moved *)

    [<RequireQualifiedAccess>]
    module Moved =

        (* Key *)

        let private key k =
            Key.add [ "moved" ] (key k)

        (* Types *)

        type private Moved =
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
            { Gone: Value<bool> option
              MovedTemporarily: Value<bool> option
              MovedPermanently: Value<bool> option }

            static member gone_ =
                (fun x -> x.Gone), (fun g x -> { x with Decisions.Gone = g })

            static member movedTemporarily_ =
                (fun x -> x.MovedTemporarily), (fun m x -> { x with MovedTemporarily = m })

            static member movedPermanently_ =
                (fun x -> x.MovedPermanently), (fun m x -> { x with Decisions.MovedPermanently = m })

            static member empty =
                { Gone = None
                  MovedTemporarily = None
                  MovedPermanently = None }

         and private Terminals =
            { Gone: Handler option
              TemporaryRedirect: Handler option
              MovedPermanently: Handler option }

            static member gone_ =
                (fun x -> x.Gone), (fun g x -> { x with Terminals.Gone = g })

            static member temporaryRedirect_ =
                (fun x -> x.TemporaryRedirect), (fun t x -> { x with TemporaryRedirect = t })

            static member movedPermanently_ =
                (fun x -> x.MovedPermanently), (fun m x -> { x with Terminals.MovedPermanently = m })

            static member empty =
                { Gone = None
                  TemporaryRedirect = None
                  MovedPermanently = None }

        (* Optics *)

        let private moved_ =
            Configuration.element_ Moved.empty [ "http"; "specifications"; "responses"; "moved" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    moved_
                >-> Moved.terminals_

            let gone_ =
                    terminals_
                >-> Terminals.gone_

            let temporaryRedirect_ =
                    terminals_
                >-> Terminals.temporaryRedirect_

            let movedPermanently_ =
                    terminals_
                >-> Terminals.movedPermanently_

            let gone k =
                Terminal.create (key k, "gone")
                    (function | _ -> Operations.gone)
                    (function | Get gone_ x -> x)

            let temporaryRedirect k =
                Terminal.create (key k, "temporary-redirect")
                    (function | _ -> Operations.temporaryRedirect)
                    (function | Get temporaryRedirect_ x -> x) 

            let movedPermanently k =
                Terminal.create (key k, "moved-permanently")
                    (function | _ -> Operations.movedPermanently)
                    (function | Get movedPermanently_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let private decisions_ =
                    moved_
                >-> Moved.decisions_

            let gone_ =
                    decisions_
                >-> Decisions.gone_

            let movedTemporarily_ =
                    decisions_
                >-> Decisions.movedTemporarily_

            let movedPermanently_ =
                    decisions_
                >-> Decisions.movedPermanently_

            let rec gone k s =
                Decision.create (key k, "see-other")
                    (function | TryGetOrElse gone_ (Static false) x -> x)
                    (movedTemporarily k s, Terminals.gone k)

            and movedTemporarily k s =
                Decision.create (key k, "found")
                    (function | TryGetOrElse movedTemporarily_ (Static false) x -> x)
                    (movedPermanently k s, Terminals.temporaryRedirect k)

            and movedPermanently k s =
                Decision.create (key k, "see-other")
                    (function | TryGetOrElse movedPermanently_ (Static false) x -> x)
                    (s, Terminals.movedPermanently k)

        (* Specification *)

        let specification =
            Decisions.gone

    (* Options *)

    [<RequireQualifiedAccess>]
    module Options =

        (* Key *)

        let private key k =
            Key.add [ "options" ] (key k)

        (* Types *)

        type private Options =
            { Terminals: Terminals }

            static member terminals_ =
                (fun x -> x.Terminals), (fun t x -> { x with Terminals = t })

            static member empty =
                { Terminals = Terminals.empty }

         and private Terminals =
            { Options: Handler option }

            static member options_ =
                (fun x -> x.Options), (fun n x -> { x with Options = n })

            static member empty =
                { Options = None }

        (* Optics *)

        let private options_ =
            Configuration.element_ Options.empty [ "http"; "specifications"; "responses"; "options" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    options_
                >-> Options.terminals_

            let options_ =
                    terminals_
                >-> Terminals.options_

            let options k =
                Terminal.create (key k, "options")
                    (function | _ -> Operations.options)
                    (function | Get options_ x -> x)

        (* Specification *)

        let specification =
            Terminals.options

    (* Other *)

    [<RequireQualifiedAccess>]
    module Other =

        (* Key *)

        let private key k =
            Key.add [ "other" ] (key k)

        (* Types *)

        type private Other =
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
            { SeeOther: Value<bool> option
              Found: Value<bool> option
              MultipleChoices: Value<bool> option }

            static member seeOther_ =
                (fun x -> x.SeeOther), (fun s x -> { x with Decisions.SeeOther = s })

            static member found_ =
                (fun x -> x.Found), (fun f x -> { x with Decisions.Found = f })

            static member multipleChoices_ =
                (fun x -> x.MultipleChoices), (fun m x -> { x with Decisions.MultipleChoices = m })

            static member empty =
                { SeeOther = None
                  Found = None
                  MultipleChoices = None }

         and private Terminals =
            { SeeOther: Handler option
              Found: Handler option
              MultipleChoices: Handler option }

            static member seeOther_ =
                (fun x -> x.SeeOther), (fun s x -> { x with Terminals.SeeOther = s })

            static member found_ =
                (fun x -> x.Found), (fun f x -> { x with Terminals.Found = f })

            static member multipleChoices_ =
                (fun x -> x.MultipleChoices), (fun m x -> { x with Terminals.MultipleChoices = m })

            static member empty =
                { SeeOther = None
                  Found = None
                  MultipleChoices = None }

        (* Optics *)

        let private other_ =
            Configuration.element_ Other.empty [ "http"; "specifications"; "responses"; "other" ]

        (* Terminals *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Terminals =

            let private terminals_ =
                    other_
                >-> Other.terminals_

            let seeOther_ =
                    terminals_
                >-> Terminals.seeOther_

            let found_ =
                    terminals_
                >-> Terminals.found_

            let multipleChoices_ =
                    terminals_
                >-> Terminals.multipleChoices_

            let seeOther k =
                Terminal.create (key k, "see-other")
                    (function | _ -> Operations.seeOther)
                    (function | Get seeOther_ x -> x) 

            let found k =
                Terminal.create (key k, "found")
                    (function | _ -> Operations.found)
                    (function | Get found_ x -> x)

            let multipleChoices k =
                Terminal.create (key k, "multiple-choices")
                    (function | _ -> Operations.multipleChoices)
                    (function | Get multipleChoices_ x -> x)

        (* Decisions *)

        [<RequireQualifiedAccess>]
        [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Decisions =

            let private decisions_ =
                    other_
                >-> Other.decisions_

            let seeOther_ =
                    decisions_
                >-> Decisions.seeOther_

            let found_ =
                    decisions_
                >-> Decisions.found_

            let multipleChoices_ =
                    decisions_
                >-> Decisions.multipleChoices_

            let rec seeOther k s =
                Decision.create (key k, "see-other")
                    (function | TryGetOrElse seeOther_ (Static false) x -> x)
                    (found k s, Terminals.seeOther k)

            and found k s =
                Decision.create (key k, "found")
                    (function | TryGetOrElse found_ (Static false) x -> x)
                    (multipleChoices k s, Terminals.found k)

            and multipleChoices k s =
                Decision.create (key k, "see-other")
                    (function | TryGetOrElse multipleChoices_ (Static false) x -> x)
                    (s, Terminals.multipleChoices k)

        (* Specification *)

        let specification =
            Decisions.seeOther
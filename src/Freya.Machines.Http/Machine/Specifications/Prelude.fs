namespace Freya.Machines.Http.Machine.Specifications

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Semantics
open Hephaestus

(* Prelude *)

(* Types *)

type Handler =
    Acceptable -> Freya<Representation>

(* Aliases *)

[<AutoOpen>]
module internal Aliases =

    (* Monadic *)

    let apply =
        Freya.Value.apply

    let bind =
        Value.Freya.bind

    let lift =
        Freya.Value.lift

    let map =
        Value.Freya.map

    (* Content *)

    let negotiable =
        Negotiation.negotiable

(* Defaults *)

[<RequireQualifiedAccess>]
module internal Defaults =

    let methods =
        set [
            GET
            HEAD
            OPTIONS ]

(* Key

   Functions for working with Hephaestus Keys, making defining and using keys
   slightly more pleasant. *)

[<RequireQualifiedAccess>]
module internal Key =

    let add x =
        Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) x)

    let root =
        add [ "http" ] Key.empty

(* Resource *)

[<RequireQualifiedAccess>]
module internal Resource =

    let private charsets c =
            Freya.Value.liftOption (c ^. Properties.Representation.charsetsSupported_)

    let private encodings c =
            Freya.Value.liftOption (c ^. Properties.Representation.contentCodingsSupported_)

    let private mediaTypes c =
            Freya.Value.liftOption (c ^. Properties.Representation.mediaTypesSupported_)

    let private languages c =
            Freya.Value.liftOption (c ^. Properties.Representation.languagesSupported_)

    let available c : Freya<Available> =
            fun charsets encodings mediaTypes languages ->
                { Charsets = charsets
                  Encodings = encodings
                  MediaTypes = mediaTypes
                  Languages = languages }
        <!> charsets c
        <*> encodings c
        <*> mediaTypes c
        <*> languages c

(* Decision

    Construction functions for building Decisions, either with a basic
    approach, or a more opinionated approach of drawing a possible
    decision from the configuration (using a supplied lens). In the
    opionated case, if the decision is not found in configuration, a
    static decision will be created from the supplied default value. *)

[<RequireQualifiedAccess>]
module internal Decision =

    let private suffix =
        sprintf "%s-decision"

    let create (key, name) decision =
        Specification.Decision.create
            (Key.add [ suffix name ] key)
            (decision >> Decision.map)

(* Terminal

   Construction functions for building Terminals, given a lens to the expected
   handler in the configuration, and an operation to apply prior to invoking
   the found handler (or invoking singly, in the case where a handler is not
   found in the configuration). *)

[<RequireQualifiedAccess>]
module internal Terminal =

    let private suffix =
        sprintf "%s-terminal"

    let create (key, name) operation handler =
        Specification.Terminal.create
            (Key.add [ suffix name ] key)
            (fun configuration ->

                let operation =
                    operation configuration

                let handler =
                    match handler configuration with
                    | Some handler -> Representation.represent (Resource.available configuration) handler
                    | _ -> Freya.empty

                operation *> handler)
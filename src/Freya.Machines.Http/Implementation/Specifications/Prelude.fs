namespace Freya.Machines.Http.Implementation.Specifications

open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Implementation.Configuration
open Freya.Machines.Http.Semantics
open Hephaestus

(* Prelude *)

(* Types *)

type Handler =
    Acceptable -> Freya<Representation>

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

(* Terminal

   Construction functions for building Terminals, given a lens to the expected
   handler in the configuration, and an operation to apply prior to invoking
   the found handler (or invoking singly, in the case where a handler is not
   found in the configuration). *)

[<RequireQualifiedAccess>]
module internal Terminal =

    let private append =
        sprintf "%s-terminal"

    let create (key, name) handler operation =
        Specification.Terminal.create
            (Key.add [ append name ] key)
            (fun configuration ->
                let handler =
                    match handler configuration with
                    | Some handler -> Representation.represent (Resource.available configuration) handler
                    | _ -> Freya.empty

                operation *> handler)
namespace Freya.Machines

open Aether
open Freya.Core
open Hephaestus

(* Key

   Functions for working with Hephaestus Keys, making defining and using keys
   slightly more pleasant. *)

[<RequireQualifiedAccess>]
module  Key =

    let add x =
        Optic.map (Lens.ofIsomorphism Key.key_) ((flip List.append) x)

    let root k =
        Key [ k ]

(* Patterns

   Commonly useful active recognizers for working with lens based access to
   data structures, particularly useful here for making access to configuration
   more concise. *)

[<AutoOpen>]
module Patterns =

    let inline (|Get|) optic =
        Optic.get optic

    let inline (|TryGet|_|) optic =
        Optic.get optic
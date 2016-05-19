namespace Freya.Machines

open Aether

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
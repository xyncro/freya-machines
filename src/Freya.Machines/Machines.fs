module Freya.Machines

open Freya.Core
open Freya.Core.Operators
open Hephaestus

(* Types

   Common types for functions, etc. as part of HTTP machines. The
   Decision<'a> type is analogous to the basic Hephaestus decision type, and
   is used by Freya users, but mapped to the Hephaestus decision type before
   the specifications for the components and models are used. *)

type Decision<'a> =
    | Function of Freya<'a>
    | Literal of 'a

(* Decisions

   Type conversions between Hephaestus specific types (the convention of
   Right = true and Left = false is followed here, Hephaestus has a value
   neutral decision scheme) and Freya types (particularly the mapping of the
   two Decision<'a> implementations). *)

[<RequireQualifiedAccess>]
module internal Decision =

    type private Decision =
        Hephaestus.Decision<State>

    let private convert =
        function | true -> Right
                 | _ -> Left

    let map =
        function | Literal l -> Decision.Literal (convert l)
                 | Function f -> Decision.Function (convert <!> f)
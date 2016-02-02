module Freya.Machines

open Freya.Core
open Freya.Core.Operators
open Hephaestus

(* Types *)

type Decision<'a> =
    | Function of Freya<'a>
    | Literal of 'a

(* Decisions *)

[<RequireQualifiedAccess>]
module Decision =

    type private Decision =
        Hephaestus.Decision<State>

    let private convert =
        function | true -> Right
                 | _ -> Left

    let map =
        function | Literal l -> Decision.Literal (convert l)
                 | Function f -> Decision.Function (convert <!> f)
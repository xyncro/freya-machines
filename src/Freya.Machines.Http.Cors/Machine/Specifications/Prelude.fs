namespace Freya.Machines.Http.Cors.Machine.Specifications

open System
open Freya.Machines
open Hephaestus

(* Decision

    Construction functions for building Decisions, either with a basic
    approach, or a more opinionated approach of drawing a possible
    decision from the configuration (using a supplied lens). In the
    opionated case, if the decision is not found in configuration, a
    static decision will be created from the supplied default value. *)

[<RequireQualifiedAccess>]
module internal Decision =

    let create (key, name) decision =
        Specification.Decision.create
            (Key.add [ name ] key)
            (decision >> Decision.map)

[<RequireQualifiedAccess>]
module String =

    let equalsIgnoreCase s2 (s1: string) =
        s1.Equals (s2, StringComparison.OrdinalIgnoreCase)
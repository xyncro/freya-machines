namespace Freya.Machines.Http.Machine.Specifications

open Freya.Core.Operators
open Freya.Machines
open Freya.Machines.Http.Machine.Configuration
open Freya.Optics.Http

(* Method

   Decision determining whether the method matches any of a supplied
   list of methods given as part of parameterization of this element.

   The element does not result in a response, only in control flow of
   the machine, and as such must be provided with both left and right
   cases (no terminals are implied).

   NOTE: This decision has a significant optimization, in that it will
   become a Static value of false if the method to be matched is not
   an allowed method for this resource. This aids significantly in
   graph optimization, but does imply that a method validation check
   should be part of the workflow (i.e. that it should correctly be
   preceded by a Validation element). *)

[<RequireQualifiedAccess>]
module internal Method =

    (* Key *)

    let private key p =
        Key.add [ p; "method" ] Key.root

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decisions =

        let rec methodMatches p methods =
            Decision.create (key p, "method-matches")
                (function | TryGet Properties.Request.methods_ (Static x) when disjoint methods x -> Static false
                          | TryGet Properties.Request.methods_ _ -> Dynamic (matches methods)
                          | _ when disjoint methods Defaults.methods -> Static false
                          | _ -> Dynamic (matches methods))

        and private disjoint s =
                Set.intersect s
             >> Set.isEmpty

        and private matches s =
                function | x when Set.contains x s -> true
                         | _ -> false
            <!> !. Request.method_

    (* Specification *)

    let specification =
        Decisions.methodMatches
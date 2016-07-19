namespace Freya.Machines.Http.Patch

open Freya.Core
open Freya.Core.Operators
open Freya.Optics.Http.Patch
open Freya.Types.Http.Patch

(* Operations *)

[<RequireQualifiedAccess>]
module Operations =

    (* Operations

       Operations for setting PATCH related response headers, including logic
       to determine permutation when the input data is potentially of varying
       shapes, and may have dependent logic. *)

    (* Accept Patch *)

    let rec acceptPatch mediaTypes =
        acceptPatchPermutations mediaTypes

    and private acceptPatchPermutations =
        function | x when Set.isEmpty x -> Freya.empty 
                 | x -> Response.Headers.acceptPatch_ .= Some (AcceptPatch (Set.toList x))

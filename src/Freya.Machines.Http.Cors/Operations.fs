namespace Freya.Machines.Http.Cors

open Arachne.Http.Cors
open Freya.Core.Operators
open Freya.Optics.Http.Cors

(* Operations *)

[<RequireQualifiedAccess>]
module Operations =

    let private accessControlAllowOrigin =
        function | x -> Response.Headers.accessControlAllowOrigin_ .= Some (AccessControlAllowOrigin (Origins x))

    let simple origin =
        accessControlAllowOrigin origin

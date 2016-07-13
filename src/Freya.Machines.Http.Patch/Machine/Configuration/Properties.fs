namespace Freya.Machines.Http.Patch.Machine.Configuration

open Aether.Operators
open Freya.Machines
open Freya.Types.Http

(* Properties *)

[<RequireQualifiedAccess>]
module Properties =

    (* Types *)

    type private Properties =
        { Request: Request }

        static member request_ =
            (fun x -> x.Request), (fun r x -> { x with Request = r })

        static member empty =
            { Request = Request.empty }

     and private Request =
        { MediaTypes: Value<MediaType list> option }

        static member mediaTypes_ =
            (fun x -> x.MediaTypes), (fun m x -> { x with MediaTypes = m })

        static member empty =
            { MediaTypes = None }

    (* Optics *)

    let private properties_ =
        Configuration.element_ Properties.empty [ "http-patch"; "configuration"; "properties" ]

    (* Resource *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Request =

        let private request_ =
                properties_
            >-> Properties.request_

        let mediaTypes_ =
                request_
            >-> Request.mediaTypes_
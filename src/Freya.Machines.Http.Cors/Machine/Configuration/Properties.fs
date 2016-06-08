namespace Freya.Machines.Http.Cors.Machine.Configuration

open Aether.Operators
open Freya.Machines

(* Properties *)

[<RequireQualifiedAccess>]
module Properties =

    (* Types *)

    type private Properties =
        { Resource: Resource }

        static member resource_ =
            (fun x -> x.Resource), (fun r x -> { x with Resource = r })

        static member empty =
            { Resource = Resource.empty}

     and private Resource =
        { AllowedOrigins: string list option }

        static member allowedOrigins_ =
            (fun x -> x.AllowedOrigins), (fun a x -> { x with AllowedOrigins = a })

        static member empty =
            { AllowedOrigins = None }

    (* Optics *)

    let private properties_ =
        Configuration.element_ Properties.empty [ "http-cors"; "configuration"; "properties" ]

    (* Resource *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Resource =

        let private resource_ =
                properties_
            >-> Properties.resource_

        let allowedOrigins_ =
                resource_
            >-> Resource.allowedOrigins_
namespace Freya.Machines.Http.Cors.Machine.Configuration

open Aether.Operators
open Arachne.Http.Cors
open Freya.Machines

(* Properties *)

[<RequireQualifiedAccess>]
module Properties =

    (* Types *)

    type private Properties =
        { Extension: Extension
          Resource: Resource }

        static member extension_ =
            (fun x -> x.Extension), (fun e x -> { x with Extension = e })

        static member resource_ =
            (fun x -> x.Resource), (fun r x -> { x with Resource = r })

        static member empty =
            { Extension = Extension.empty
              Resource = Resource.empty }

    and private Extension =
        { Enabled: Value<bool> option }

        static member enabled_ =
            (fun x -> x.Enabled), (fun e x -> { x with Enabled = e })

        static member empty =
            { Enabled = None }

     and private Resource =
        { Origins: Value<SerializedOrigin list> option }

        static member origins_ =
            (fun x -> x.Origins), (fun a x -> { x with Origins = a })

        static member empty =
            { Origins = None }

    (* Optics *)

    let private properties_ =
        Configuration.element_ Properties.empty [ "http-cors"; "configuration"; "properties" ]

    (* Extension *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Extension =

        let private extension_ =
                properties_
            >-> Properties.extension_

        let enabled_ =
                extension_
            >-> Extension.enabled_

    (* Resource *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Resource =

        let private resource_ =
                properties_
            >-> Properties.resource_

        let origins_ =
                resource_
            >-> Resource.origins_
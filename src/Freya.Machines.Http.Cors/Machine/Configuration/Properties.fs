namespace Freya.Machines.Http.Cors.Machine.Configuration

open System
open Aether.Operators
open Freya.Machines
open Freya.Types.Http
open Freya.Types.Http.Cors

(* Properties *)

[<RequireQualifiedAccess>]
module Properties =

    (* Types *)

    type private Properties =
        { Resource: Resource }

        static member resource_ =
            (fun x -> x.Resource), (fun r x -> { x with Resource = r })

        static member empty =
            { Resource = Resource.empty }

     and private Resource =
        { Origins: Value<Set<SerializedOrigin>> option
          Methods: Value<Set<Method>> option
          Headers: Value<Set<string>> option
          ExposedHeaders: Value<Set<string>> option
          SupportsCredentials: Value<bool> option
          MaxAge: Value<TimeSpan> option }

        static member origins_ =
            (fun x -> x.Origins), (fun o x -> { x with Origins = o })

        static member methods_ =
            (fun x -> x.Methods), (fun m x -> { x with Methods = m })

        static member headers_ =
            (fun x -> x.Headers), (fun h x -> { x with Headers = h })

        static member exposedHeaders_ =
            (fun x -> x.ExposedHeaders), (fun e x -> { x with ExposedHeaders = e })

        static member supportsCredentials_ =
            (fun x -> x.SupportsCredentials), (fun s x -> { x with SupportsCredentials = s })

        static member maxAge_ =
            (fun x -> x.MaxAge), (fun m x -> { x with MaxAge = m })

        static member empty =
            { Origins = None
              Methods = None
              Headers = None
              ExposedHeaders = None
              SupportsCredentials = None
              MaxAge = None }

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

        let origins_ =
                resource_
            >-> Resource.origins_

        let methods_ =
                resource_
            >-> Resource.methods_

        let headers_ =
                resource_
            >-> Resource.headers_

        let exposedHeaders_ =
                resource_
            >-> Resource.exposedHeaders_

        let supportsCredentials_ =
                resource_
            >-> Resource.supportsCredentials_

        let maxAge_ =
                resource_
            >-> Resource.maxAge_
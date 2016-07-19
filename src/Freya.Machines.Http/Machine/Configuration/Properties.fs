namespace Freya.Machines.Http.Machine.Configuration

open System
open Aether.Operators
open Freya.Machines
open Freya.Types.Http
open Freya.Types.Language

(* Properties

   Properties of the resource which the machine represents, defined as a
   distinct set of types as these may be used/shared by any of the many
   elements defined as part of an HTTP machine. *)

[<RequireQualifiedAccess>]
module Properties =

    (* Types *)

    type private Properties =
        { Request: Request
          Representation: Representation
          Resource: Resource }

        static member request_ =
            (fun x -> x.Request), (fun r x -> { x with Request = r })

        static member representation_ =
            (fun x -> x.Representation), (fun r x -> { x with Representation = r })

        static member resource_ =
            (fun x -> x.Resource), (fun r x -> { x with Resource = r })

        static member empty =
            { Request = Request.empty
              Representation = Representation.empty
              Resource = Resource.empty }

     and private Request =
        { MediaTypes: Value<Set<MediaType>> option
          Methods: Value<Set<Method>> option }

        static member mediaTypes_ =
            (fun x -> x.MediaTypes), (fun m x -> { x with Request.MediaTypes = m })

        static member methods_ =
            (fun x -> x.Methods), (fun m x -> { x with Methods = m })

        static member empty =
            { MediaTypes = None
              Methods = None }

     and private Representation =
        { MediaTypes: Value<Set<MediaType>> option
          Languages: Value<Set<LanguageTag>> option
          Charsets: Value<Set<Charset>> option
          ContentCodings: Value<Set<ContentCoding>> option }

        static member mediaTypes_ =
            (fun x -> x.MediaTypes), (fun m x -> { x with Representation.MediaTypes = m })

        static member languages_ =
            (fun x -> x.Languages), (fun l x -> { x with Languages = l })

        static member charsets_ =
            (fun x -> x.Charsets), (fun c x -> { x with Charsets = c })

        static member contentCodings_ =
            (fun x -> x.ContentCodings), (fun c x -> { x with ContentCodings = c })

        static member empty =
            { MediaTypes = None
              Languages = None
              Charsets = None
              ContentCodings = None }

     and private Resource =
        { EntityTag: Value<EntityTag> option
          LastModified: Value<DateTime> option }

        static member entityTag_ =
            (fun x -> x.EntityTag), (fun e x -> { x with EntityTag = e })

        static member lastModified_ =
            (fun x -> x.LastModified), (fun l x -> { x with LastModified = l })

        static member empty =
            { EntityTag = None
              LastModified = None }

    (* Optics *)

    let private properties_ =
        Configuration.element_ Properties.empty [ "http"; "configuration"; "properties" ]

    (* Request *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Request =

        let private request_ =
                properties_
            >-> Properties.request_

        let mediaTypes_ =
                request_
            >-> Request.mediaTypes_

        let methods_ =
                request_
            >-> Request.methods_

    (* Representation *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Representation =

        let private representation_ =
                properties_
            >-> Properties.representation_

        let charsets_ =
                representation_
            >-> Representation.charsets_

        let contentCodings_ =
                representation_
            >-> Representation.contentCodings_

        let languages_ =
                representation_
            >-> Representation.languages_

        let mediaTypes_ =
                representation_
            >-> Representation.mediaTypes_

    (* Resource *)

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Resource =

        let private resource_ =
                properties_
            >-> Properties.resource_

        let entityTag_ =
                resource_
            >-> Resource.entityTag_

        let lastModified_ =
                resource_
            >-> Resource.lastModified_
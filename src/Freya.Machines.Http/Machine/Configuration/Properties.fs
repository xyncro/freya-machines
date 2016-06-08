namespace Freya.Machines.Http.Machine.Configuration

open System
open Aether.Operators
open Arachne.Http
open Arachne.Language
open Freya.Machines

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
        { Methods: Value<Set<Method>> option }

        static member methods_ =
            (fun x -> x.Methods), (fun m x -> { x with Methods = m })

        static member empty =
            { Methods = None }

     and private Representation =
        { MediaTypesSupported: Value<MediaType list> option
          LanguagesSupported: Value<LanguageTag list> option
          CharsetsSupported: Value<Charset list> option
          ContentCodingsSupported: Value<ContentCoding list> option }

        static member mediaTypesSupported_ =
            (fun x -> x.MediaTypesSupported), (fun m x -> { x with MediaTypesSupported = m })

        static member languagesSupported_ =
            (fun x -> x.LanguagesSupported), (fun l x -> { x with LanguagesSupported = l })

        static member charsetsSupported_ =
            (fun x -> x.CharsetsSupported), (fun c x -> { x with CharsetsSupported = c })

        static member contentCodingsSupported_ =
            (fun x -> x.ContentCodingsSupported), (fun c x -> { x with ContentCodingsSupported = c })

        static member empty =
            { MediaTypesSupported = None
              LanguagesSupported = None
              CharsetsSupported = None
              ContentCodingsSupported = None }

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

        let charsetsSupported_ =
                representation_
            >-> Representation.charsetsSupported_

        let contentCodingsSupported_ =
                representation_
            >-> Representation.contentCodingsSupported_

        let languagesSupported_ =
                representation_
            >-> Representation.languagesSupported_

        let mediaTypesSupported_ =
                representation_
            >-> Representation.mediaTypesSupported_

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
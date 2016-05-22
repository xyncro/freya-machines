namespace Freya.Machines.Http

open System
open Arachne.Http
open Arachne.Language
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Hephaestus

(* Inference

   Inference modules and functions for commonly inferred types, when not
   defined elsewhere. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Charsets =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member Charsets (x: Freya<Charset list>) =
                Dynamic x

            static member Charsets (x: Charset list) =
                Static x

            static member Charsets (x: Charset) =
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Charsets: ^a -> Value<Charset list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Components =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member Components (x: Set<Component<Configuration,unit,State>> list) =
                Set.unionMany x

            static member Components (x: Set<Component<Configuration,unit,State>>) =
                x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Components: ^a -> Set<Component<Configuration,unit,State>>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ContentCodings =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member ContentCodings (x: Freya<ContentCoding list>) =
                Dynamic x

            static member ContentCodings (x: ContentCoding list) =
                Static x

            static member ContentCodings (x: ContentCoding) =
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member ContentCodings: ^a -> Value<ContentCoding list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DateTime =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member DateTime (x: Freya<DateTime>) =
                Dynamic x

            static member DateTime (x: DateTime) =
                Static x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member DateTime: ^a -> Value<DateTime>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Decision =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member Decision (x: Freya<bool>) =
                Dynamic x

            static member Decision (x: bool) =
                Static x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Decision: ^a -> Value<bool>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EntityTag =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member ETag (x: Freya<EntityTag>) =
                Dynamic x

            static member ETag (x: EntityTag) =
                Static x

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member ETag: ^a -> Value<EntityTag>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Handler =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member inline Handler (x: Acceptable -> Freya<Representation>) =
                x

            static member inline Handler (x: Freya<Representation>) =
                fun (_: Acceptable) -> x

            static member inline Handler (x: Representation) =
                fun (_: Acceptable) -> Freya.init x

        let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member Handler: ^a -> (Acceptable -> Freya<Representation>)) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module LanguageTags =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member LanguageTags (x: Freya<LanguageTag list>) =
                Dynamic x

            static member LanguageTags (x: LanguageTag list) =
                Static x

            static member LanguageTags (x: LanguageTag) =
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member LanguageTags: ^a -> Value<LanguageTag list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MediaTypes =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member MediaTypes (x: Freya<MediaType list>) =
                Dynamic x

            static member MediaTypes (x: MediaType list) =
                Static x

            static member MediaTypes (x: MediaType) =
                Static [ x ]

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member MediaTypes: ^a -> Value<MediaType list>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Methods =

    (* Inference *)

    [<RequireQualifiedAccess>]
    module Inference =

        type Defaults =
            | Defaults

            static member Methods (x: Freya<Method list>) =
                Dynamic (Set.ofList <!> x)

            static member Methods (x: Method list) =
                Static (Set.ofList x)

            static member Methods (x: Method) =
                Static (Set.singleton x)

        let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Methods: ^a -> Value<Set<Method>>) a)

        let inline infer (x: 'a) =
            defaults (x, Defaults)

    let inline infer v =
        Inference.infer v
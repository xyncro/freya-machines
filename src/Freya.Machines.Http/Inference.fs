namespace Freya.Machines.Http

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Machines
open Freya.Types.Http
open Freya.Types.Language
open Hephaestus

(* Inference

   Inference modules and functions for commonly inferred types, when not
   defined elsewhere. *)

[<AutoOpen>]
module Inference =

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Charsets =

        (* Inference *)

        [<RequireQualifiedAccess>]
        module Inference =

            type Defaults =
                | Defaults

                static member Charsets (x: Freya<Charset list>) =
                    Dynamic (Set.ofList <!> x)

                static member Charsets (x: Charset list) =
                    Static (Set.ofList x)

                static member Charsets (x: Charset) =
                    Static (Set.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member Charsets: ^a -> Value<Set<Charset>>) a)

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
                    Dynamic (Set.ofList <!> x)

                static member ContentCodings (x: ContentCoding list) =
                    Static (Set.ofList x)

                static member ContentCodings (x: ContentCoding) =
                    Static (Set.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member ContentCodings: ^a -> Value<Set<ContentCoding>>) a)

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
                    Dynamic (Set.ofList <!> x)

                static member LanguageTags (x: LanguageTag list) =
                    Static (Set.ofList x)

                static member LanguageTags (x: LanguageTag) =
                    Static (Set.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member LanguageTags: ^a -> Value<Set<LanguageTag>>) a)

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
                    Dynamic (Set.ofList <!> x)

                static member MediaTypes (x: MediaType list) =
                    Static (Set.ofList x)

                static member MediaTypes (x: MediaType) =
                    Static (Set.singleton x)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member MediaTypes: ^a -> Value<Set<MediaType>>) a)

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

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Operation =

        (* Inference *)

        [<RequireQualifiedAccess>]
        module Inference =

            type Defaults =
                | Defaults

                static member Operation (x: Freya<bool>) =
                    x

                static member Operation (x: Freya<unit>) =
                    Freya.map (x, fun _ -> true)

            let inline defaults (a: ^a, _: ^b) =
                ((^a or ^b) : (static member Operation: ^a -> Freya<bool>) a)

            let inline infer (x: 'a) =
                defaults (x, Defaults)

        let inline infer v =
            Inference.infer v
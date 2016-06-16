namespace Freya.Machines.Http

open System

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module FreyaMachine =

        [<Obsolete ("Explicit conversion to a Freya Pipeline is no longer required in Freya.")>]
        let toPipeline =
            id

    [<AutoOpen>]
    module Syntax =

        (* Request *)

        type HttpMachineBuilder with

            [<Obsolete ("Use methods instead (logic for known methods has been simplified).")>]
            [<CustomOperation ("methodsKnown", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.MethodsKnown (m, _) = 
                x.Bind (m, fun _ -> m)

            [<Obsolete ("Use methods instead.")>]
            [<CustomOperation ("methodsSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.MethodsSupported (m, _) = 
                x.Bind (m, fun _ -> m)

        (* Representation *)

        type HttpMachineBuilder with

            [<Obsolete ("Use charsets instead.")>]
            [<CustomOperation ("charsetsSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.CharsetsSupported (m, a) =
                x.Charsets (m, a)

            [<Obsolete ("Use contentCodings instead.")>]
            [<CustomOperation ("contentCodingsSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.ContentCodingsSupported (m, a) =
                x.ContentCodings (m, a)

            [<Obsolete ("Use languages instead.")>]
            [<CustomOperation ("languagesSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.LanguagesSupported (m, a) =
                x.Languages (m, a)

            [<Obsolete ("Use mediaTypes instead.")>]
            [<CustomOperation ("mediaTypesSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.MediaTypesSupported (m, a) =
                x.MediaTypes (m, a)

        (* Operations *)

        type HttpMachineBuilder with

            [<Obsolete ("Patch is no longer part of the HTTP Machine. Use the Patch support extension.")>]
            [<CustomOperation ("doPatch", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.DoPatch (m, _) = 
                x.Bind (m, fun _ -> m)
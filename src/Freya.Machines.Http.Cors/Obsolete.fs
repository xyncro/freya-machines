namespace Freya.Machines.Http.Cors

open System
open Freya.Machines.Http

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module FreyaMachine =

        [<Obsolete ("Explicit conversion to a Freya Pipeline is no longer required in Freya.")>]
        let toPipeline =
            id

    [<AutoOpen>]
    module Syntax =

        (* Properties *)

        type HttpMachineBuilder with

            [<Obsolete ("Use corsExposedHeaders instead.")>]
            [<CustomOperation ("corsHeadersExposed", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.CorsHeadersExposed (m, a) =
                x.CorsExposedHeaders (m, a)

            [<Obsolete ("Use corsHeaders instead.")>]
            [<CustomOperation ("corsHeadersSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.CorsHeadersSupported (m, a) = 
                x.CorsHeaders (m, a)

            [<Obsolete ("Use corsMethods (if different to methods).")>]
            [<CustomOperation ("corsMethodsSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.CorsMethodsSupported (m, a) = 
                x.CorsMethods (m, a)

            [<Obsolete ("Use corsMethods instead.")>]
            [<CustomOperation ("corsOriginsSupported", MaintainsVariableSpaceUsingBind = true)>]
            member inline x.CorsOriginsSupported (m, a) = 
                x.CorsOrigins (m, a)
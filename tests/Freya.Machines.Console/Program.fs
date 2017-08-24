module Freya.Machines.Console

open System

// Freya

open Freya.Core
open Freya.Machines.Http
open Freya.Machines.Http.Cors
open Freya.Machines.Http.Patch
open Freya.Types.Http
open Freya.Types.Http.Cors
open Freya.Types.Http.Patch
open Freya.Types.Language

let ok =
    { Data = "Hello World!"B
      Description =
        { Charset = Some Charset.Utf8
          Encodings = None
          MediaType = Some MediaType.Text
          Languages = Some [ LanguageTag.parse "en-gb"
                             LanguageTag.parse "en" ] } }

let machine =
    freyaHttpMachine {
        // methods PATCH

        // patch
        // patchEnabled false }


       acceptableMediaTypes [ MediaType.parse "text/plain" ]
       availableMediaTypes [ MediaType.parse "text/plain" ]
       methods [ DELETE; GET; OPTIONS; PATCH ]

       handleOk ok

       cors
       corsEnabled true
       corsMaxAge 3600
       corsOrigins [ SerializedOrigin.parse "http://xyncro.com" ]

       patch
       patchEnabled true
       patchAcceptableMediaTypes [ MediaType.parse "text/plain" ] }

let freyaApp =
    OwinMidFunc.ofFreya machine

// Kestrel

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting

type Startup () =
    member __.Configure (app: IApplicationBuilder) : unit =
        app.UseOwin (fun p -> p.Invoke(freyaApp)) |> ignore
        ()

let server () =
    try
        WebHostBuilder()
            .UseKestrel()
            .UseUrls([| "http://localhost:7000" |])
            //.UseContentRoot(Directory.GetCurrentDirectory())
            //.UseDefaultHostingConfiguration(args)
            .UseStartup<Startup>()
            .Build()
            .Run()
    with
        | ex ->
            printfn "%A" ex

// Main

[<EntryPoint>]
let main _ =
    let _ = server ()
    0

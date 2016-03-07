open System

// Freya (and Arachne)

open Arachne.Http
open Arachne.Language
open Freya.Core
open Freya.Machines.Http

let ok =
    freya {
        return {
            Data = "Hello World!"B
            Description =
                { Charset = Some Charset.Utf8
                  Encodings = None
                  MediaType = Some MediaType.Text
                  Languages = Some [ LanguageTag.parse "en-gb"
                                     LanguageTag.parse "en" ] } } }

let machine =
    freyaMachine {
        handleOk ok }

let app =
    OwinAppFunc.ofFreya machine

// Katana

open Microsoft.Owin.Hosting

type App () =
    member __.Configuration () =
        app

let server () =
    WebApp.Start<App> "http://localhost:7000"

// Main

[<EntryPoint>]
let main _ =

    let _ = server ()
    let _ = Console.ReadLine ()

    0

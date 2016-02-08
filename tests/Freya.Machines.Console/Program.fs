open Freya.Machines
open Freya.Machines.Http

[<EntryPoint>]
let main _ =

    let machine =
        freyaMachine {
            serviceAvailable true }

    0

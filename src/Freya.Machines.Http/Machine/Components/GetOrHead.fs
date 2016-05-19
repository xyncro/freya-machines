namespace Freya.Machines.Http.Machine.Components

#nowarn "46"

open Freya.Machines
open Freya.Machines.Http.Machine.Specifications
open Freya.Machines.Http.Semantics
open Hephaestus

(* Get or Head *)

//[<RequireQualifiedAccess>]
//module GetOrHead =
//
//    (* Name *)
//
//    [<Literal>]
//    let private GetOrHead =
//        "get-or-head"
//
//    (* Export *)
//
//    let private getOrHead s =
//        Method.export GetOrHead (Set.ofList [ GET; HEAD ]) (
//            s, Existence.export GetOrHead (
//                Responses.Moved.export GetOrHead (
//                    Responses.Missing.export GetOrHead),
//                Preconditions.Common.export GetOrHead (
//                    Preconditions.Safe.export GetOrHead (
//                        Responses.Other.export GetOrHead (
//                            Responses.Common.export GetOrHead)))))
//
//    let export =
//        { Metadata =
//            { Name = "http.get"
//              Description = None }
//          Requirements =
//            { Required = set [ "http.core" ]
//              Preconditions = List.empty }
//          Operations =
//            [ Splice (Key [ "http"; "end-decision" ], Right, getOrHead) ] }
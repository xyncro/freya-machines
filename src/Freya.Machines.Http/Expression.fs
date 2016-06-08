namespace Freya.Machines.Http

open Arachne.Http
open Freya.Core
open Freya.Machines.Http.Machine.Configuration
open Freya.Machines.Http.Machine.Specifications

(* Builder

   Computation expression builder for configuring the HTTP Machine, providing a
   simple type-safe syntax and static inference based overloads of single
   functions.

   The builder uses the basic configuration builder defined in Freya.Core, which
   only requires the supply of init and bind functions of the appropriate types
   to implement a type suitable for declarative configuration. *)

type HttpMachineBuilder () =
    inherit ConfigurationBuilder<HttpMachine>
        { Init = HttpMachine.init
          Bind = HttpMachine.bind }

(* Syntax *)

[<AutoOpen>]
module Syntax =

    (* Extensions

       Custom extensions to the HTTP model. *)

    type HttpMachineBuilder with

        [<CustomOperation ("using", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Using (m, a) =
            HttpMachine.map (m, Extensions.Components.components_, (Set.union (Components.infer a)))

    (* Properties

       Configuration for common properties of a the HTTP model which may be used by
       multiple elements/components as part. Multiple implementations may rely on
       the same core declarative property of a resource without needing to be aware
       of the existence of other consumers of that property. *)

    (* Request *)

    type HttpMachineBuilder with

        [<CustomOperation ("methods", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Methods (m, a) =
            HttpMachine.set (m, Properties.Request.methods_, Methods.infer a)

    (* Representation *)

    type HttpMachineBuilder with

        [<CustomOperation ("charsetsSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.CharsetsSupported (m, a) =
            HttpMachine.set (m, Properties.Representation.charsetsSupported_, Charsets.infer a)

        [<CustomOperation ("contentCodingsSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.ContentCodingsSupported (m, a) =
            HttpMachine.set (m, Properties.Representation.contentCodingsSupported_, ContentCodings.infer a)

        [<CustomOperation ("languagesSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.LanguagesSupported (m, a) =
            HttpMachine.set (m, Properties.Representation.languagesSupported_, LanguageTags.infer a)

        [<CustomOperation ("mediaTypesSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.MediaTypesSupported (m, a) =
            HttpMachine.set (m, Properties.Representation.mediaTypesSupported_, MediaTypes.infer a)

    (* Resource *)

    type HttpMachineBuilder with

        [<CustomOperation ("entityTag", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.EntityTag (m, a) =
            HttpMachine.set (m, Properties.Resource.entityTag_, EntityTag.infer a)

        [<CustomOperation ("lastModified", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.LastModified (m, a) =
            HttpMachine.set (m, Properties.Resource.lastModified_, DateTime.infer a)

    (* Specifications

       Configuration for discrete specifications used to make up specific
       components used within the HTTP model. The elements structure is flattened
       here to make the application of custom syntax more tractable. *)

    (* Assertions *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("serviceAvailable", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.ServiceAvailable (m, decision) =
            HttpMachine.set (m, Assertions.Decisions.serviceAvailable_, Decision.infer decision)

        [<CustomOperation ("httpVersionSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HttpVersionSupported (m, decision) =
            HttpMachine.set (m, Assertions.Decisions.httpVersionSupported_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleServiceUnavailable", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleServiceUnavailable (m, handler) =
            HttpMachine.set (m, Assertions.Terminals.serviceUnavailable_, Handler.infer handler)

        [<CustomOperation ("handleNotImplemented", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleNotImplemented (m, handler) =
            HttpMachine.set (m, Assertions.Terminals.notImplemented_, Handler.infer handler)

        [<CustomOperation ("handleNotSupported", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleNotSupported (m, handler) =
            HttpMachine.set (m, Assertions.Terminals.httpVersionNotSupported_, Handler.infer handler)

    (* Conflict *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("conflict", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Conflict (m, decision) =
            HttpMachine.set (m, Conflict.Decisions.conflict_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleConflict", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleConflict (m, handler) =
            HttpMachine.set (m, Conflict.Terminals.conflict_, Handler.infer handler)

    (* Existence *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("exists", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Exists (m, decision) =
            HttpMachine.set (m, Existence.Decisions.exists_, Decision.infer decision)

    (* Fallback *)

    type HttpMachineBuilder with

        (* Terminals *)

        [<CustomOperation ("handleFallback", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleFallback (m, handler) =
            HttpMachine.set (m, Fallback.Terminals.fallback_, Handler.infer handler)

    (* Negotiations *)

    type HttpMachineBuilder with

        (* Terminals *)

        [<CustomOperation ("handleNotAcceptable", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleNotAcceptable (m, handler) =
            HttpMachine.set (m, Negotiations.Terminals.notAcceptable_, Handler.infer handler)

    (* Operations *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("completed", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Completed (m, decision) =
            HttpMachine.set (m, Operation.Decisions.completed_, Decision.infer decision)

        (* Operations *)

        [<CustomOperation ("doDelete", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.DoDelete (m, a) =
            HttpMachine.set (m, (Operation.Decisions.operationMethod_ DELETE), Operation.infer a)

        [<CustomOperation ("doPost", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.DoPost (m, a) =
            HttpMachine.set (m, (Operation.Decisions.operationMethod_ POST), Operation.infer a)

        [<CustomOperation ("doPut", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.DoPut (m, a) =
            HttpMachine.set (m, (Operation.Decisions.operationMethod_ PUT), Operation.infer a)

        (* Terminals *)

        [<CustomOperation ("handleAccepted", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleAccepted (m, handler) =
            HttpMachine.set (m, Operation.Terminals.accepted_, Handler.infer handler)

        [<CustomOperation ("handleInternalServerError", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleInternalServerError (m, handler) =
            HttpMachine.set (m, Operation.Terminals.internalServerError_, Handler.infer handler)

    (* Permissions *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("authorized", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Authorized (m, decision) =
            HttpMachine.set (m, Permissions.Decisions.authorized_, Decision.infer decision)

        [<CustomOperation ("allowed", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Allowed (m, decision) =
            HttpMachine.set (m, Permissions.Decisions.allowed_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleUnauthorized", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleUnauthorized (m, handler) =
            HttpMachine.set (m, Permissions.Terminals.unauthorized_, Handler.infer handler)

        [<CustomOperation ("handleForbidden", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleForbidden (m, handler) =
            HttpMachine.set (m, Permissions.Terminals.forbidden_, Handler.infer handler)

    (* Preconditions *)

    type HttpMachineBuilder with

        (* Terminals *)

        [<CustomOperation ("handlePreconditionFailed", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandlePreconditionFailed (m, handler) =
            HttpMachine.set (m, Preconditions.Shared.Terminals.preconditionFailed_, Handler.infer handler)

    (* Responses.Common *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("noContent", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.NoContent (m, decision) =
            HttpMachine.set (m, Responses.Common.Decisions.noContent_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleNoContent", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleNoContent (m, handler) =
            HttpMachine.set (m, Responses.Common.Terminals.noContent_, Handler.infer handler)

        [<CustomOperation ("handleOk", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleOk (m, handler) =
            HttpMachine.set (m, Responses.Common.Terminals.ok_, Handler.infer handler)

    (* Responses.Created *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("created", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Created (m, decision) =
            HttpMachine.set (m, Responses.Created.Decisions.created_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleCreated", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleCreated (m, handler) =
            HttpMachine.set (m, Responses.Created.Terminals.created_, Handler.infer handler)

    (* Responses.Missing *)

    type HttpMachineBuilder with

        (* Terminals *)

        [<CustomOperation ("handleNotFound", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleNotFound (m, handler) =
            HttpMachine.set (m, Responses.Missing.Terminals.notFound_, Handler.infer handler)

    (* Responses.Moved *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("gone", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Gone (m, decision) =
            HttpMachine.set (m, Responses.Moved.Decisions.gone_, Decision.infer decision)

        [<CustomOperation ("movedPermanently", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.MovedPermanently (m, decision) =
            HttpMachine.set (m, Responses.Moved.Decisions.movedPermanently_, Decision.infer decision)

        [<CustomOperation ("movedTemporarily", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.MovedTemporarily (m, decision) =
            HttpMachine.set (m, Responses.Moved.Decisions.movedTemporarily_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleGone", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleGone (m, handler) =
            HttpMachine.set (m, Responses.Moved.Terminals.gone_, Handler.infer handler)

        [<CustomOperation ("handleMovedPermanently", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleMovedPermanently (m, handler) =
            HttpMachine.set (m, Responses.Moved.Terminals.movedPermanently_, Handler.infer handler)

        [<CustomOperation ("handleTemporaryRedirect", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleTemporaryRedirect (m, handler) =
            HttpMachine.set (m, Responses.Moved.Terminals.temporaryRedirect_, Handler.infer handler)

    (* Responses.Options *)

    type HttpMachineBuilder with

        (* Terminals *)

        [<CustomOperation ("handleOptions", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleOptions (m, handler) =
            HttpMachine.set (m, Responses.Options.Terminals.options_, Handler.infer handler)

    (* Responses.Other *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("found", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.Found (m, decision) =
            HttpMachine.set (m, Responses.Other.Decisions.found_, Decision.infer decision)

        [<CustomOperation ("multipleChoices", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.MultipleChoices (m, decision) =
            HttpMachine.set (m, Responses.Other.Decisions.multipleChoices_, Decision.infer decision)

        [<CustomOperation ("seeOther", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.SeeOther (m, decision) =
            HttpMachine.set (m, Responses.Other.Decisions.seeOther_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleFound", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleFound (m, handler) =
            HttpMachine.set (m, Responses.Other.Terminals.found_, Handler.infer handler)

        [<CustomOperation ("handleMultipleChoices", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleMultipleChoices (m, handler) =
            HttpMachine.set (m, Responses.Other.Terminals.multipleChoices_, Handler.infer handler)

        [<CustomOperation ("handleSeeOther", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleSeeOther (m, handler) =
            HttpMachine.set (m, Responses.Other.Terminals.seeOther_, Handler.infer handler)

    (* Validations *)

    type HttpMachineBuilder with

        (* Decisions *)

        [<CustomOperation ("expectationMet", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.ExpectationMet (m, decision) =
            HttpMachine.set (m, Validations.Decisions.expectationMet_, Decision.infer decision)

        [<CustomOperation ("uriTooLong", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.UriTooLong (m, decision) =
            HttpMachine.set (m, Validations.Decisions.uriTooLong_, Decision.infer decision)

        [<CustomOperation ("badRequest", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.BadRequest (m, decision) =
            HttpMachine.set (m, Validations.Decisions.badRequest_, Decision.infer decision)

        (* Terminals *)

        [<CustomOperation ("handleExpectationFailed", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleExpectationFailed (m, handler) =
            HttpMachine.set (m, Validations.Terminals.expectationFailed_, Handler.infer handler)

        [<CustomOperation ("handleMethodNotAllowed", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleMethodNotAllowed (m, handler) =
            HttpMachine.set (m, Validations.Terminals.methodNotAllowed_, Handler.infer handler)

        [<CustomOperation ("handleUriTooLong", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleUriTooLong (m, handler) =
            HttpMachine.set (m, Validations.Terminals.uriTooLong_, Handler.infer handler)

        [<CustomOperation ("handleBadRequest", MaintainsVariableSpaceUsingBind = true)>]
        member inline __.HandleBadRequest (m, handler) =
            HttpMachine.set (m, Validations.Terminals.badRequest_, Handler.infer handler)

(* Builder

   The instance of HttpMachineBuilder which will be used to provide the
   custom computation expression syntax. The instance is aliased as freyaMachine
   for backwards compatibility with earlier versions of Freya which assumed a
   single machine implementation in perpetuity. *)

[<AutoOpen>]
module Builder =

    let freyaHttpMachine =
        HttpMachineBuilder ()

    let freyaMachine =
        freyaHttpMachine
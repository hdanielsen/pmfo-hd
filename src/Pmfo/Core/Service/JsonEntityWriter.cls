 
 /*------------------------------------------------------------------------
    File        : JsonEntityWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 20 09:35:38 EDT 2019
    Notes       : Extends return of error for non-JSDO client 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.Net.HTTP.Filter.Payload.JsonEntityWriter from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.AppError from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Reflect.DataType from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Pmfo.Core.Error.ApplicationError from propath.

class Pmfo.Core.Service.JsonEntityWriter inherits JsonEntityWriter: 

    constructor public JsonEntityWriter (  ):
        super ().
    end constructor.
    
    method public override int64 Write(input poData as Object):
        define variable oJsonWrapper as JsonObject.
         
        Assert:NotNull(poData).
        if type-of(poData, Error) then
        do:
            WriteError(cast(poData, Progress.Lang.Error)).
            if valid-object(this-object:Entity) then 
            do:
                oJsonWrapper = new JsonObject(). 
                if type-of(this-object:Entity, JsonArray) then
                   oJsonWrapper:Add("response",cast(this-object:Entity, JsonArray)).
                else 
                   oJsonWrapper:Add("response",cast(this-object:Entity, JsonObject)).
                this-object:Entity = oJsonWrapper.
            end.    
            
            return 0.
        end.
        return super:Write(poData).
    end.
    
    method protected void WriteStop(input poStop as Progress.Lang.Stop):
        define variable oResponse   as JsonObject no-undo.
        define variable oError      as JsonObject no-undo.
        define variable oErrorList  as JsonArray no-undo.
        define variable iMax        as integer no-undo.
        define variable iStart      as integer no-undo.
        define variable iLoop       as integer no-undo.
        
        Assert:NotNull(poStop, 'Stop').
        
        assign oResponse  = new JsonObject()
               oErrorList = new JsonArray()
               oError = new JsonObject()
               .
        oErrorList:Add(oError).
        oError:Add('errorMsg':u, entry(num-entries(poStop:GetClass():TypeName,"."),poStop:GetClass():TypeName,".")).
        oError:Add('errorNum':u, 0).
        oResponse:Add('errors':u, oErrorList).
        case true:
            // single error? add an simple JSON object
            when not valid-object(this-object:Entity) then
                assign this-object:Entity = oResponse.
            
            // if there's already an array, add to it                
            when type-of(this-object:Entity, JsonArray) then
                cast(this-object:Entity, JsonArray):Add(oResponse).
            
            // if there's a single json object, switch to an array
            when type-of(this-object:Entity, JsonObject) then
            do:
                assign oErrorList = new JsonArray().
                oErrorList:Add(cast(this-object:Entity, JsonObject)).
                oErrorList:Add(oResponse).
                
                assign this-object:Entity = oErrorList.
            end.
        end case.
               
        if poStop:CallStack ne ? then
        do:
            assign oErrorList = new JsonArray()
                   iMax       = num-entries(poStop:CallStack, StringConstant:LF)
                   .
            oResponse:Add('stack':u, oErrorList).
            do iLoop = 1 to iMax:
                oErrorList:Add(entry(iLoop, poStop:CallStack, StringConstant:LF)).
            end.
        end.       
    end method.    
    
    /* WriteError with proper names (no underscores) and manage 
       AppError:ReturnValue as an error. Note that this is in principle an 
       override of a super class method, but it is private. 
       To make this work we override the calling Write(Object) and calls this 
       from there if the object is error */
    method protected override void WriteError(input poError as Progress.Lang.Error):
        define variable oResponse as JsonObject no-undo.
        define variable oError as JsonObject no-undo.
        define variable oErrorList as JsonArray no-undo.
        define variable iLoop as integer no-undo.
        define variable cRetVal as character no-undo.
        define variable iMax as integer no-undo.
        define variable oProp as Progress.Reflect.Property no-undo.
        define variable oInner as Progress.Lang.Error no-undo.
        define variable oInnerStop as Progress.Lang.Stop no-undo.
        define variable istart as integer no-undo.
        define variable oComplexError as JsonConstruct no-undo.
        Assert:NotNull(poError, 'Error').
        
        /* build the current error */
        assign oResponse  = new JsonObject()
               oErrorList = new JsonArray()
               iMax       = poError:NumMessages
               iStart = 1  
               .
        
/*        if type-of(poError, ComplexError) then                              */
/*        do:                                                                 */
/*             oComplexError = cast(poError,ComplexError):JsonMessages.       */
/*             if type-of(oComplexError,JsonObject) then                      */
/*                 oResponse:Add("dataErrors",cast(oComplexError,JsonObject)).*/
/*             else                                                           */
/*                 oResponse:Add("dataErrors",cast(oComplexError,JsonArray)). */
/*        end.                                                                */
/*        else do:                                                            */
            if type-of(poError, AppError) then
            do:
                if cast(poError, AppError):ReturnValue > "" then
                do:
                   assign oError = new JsonObject().
                   oErrorList:Add(oError).
                   oError:Add('errorMsg':u, cast(poError, AppError):ReturnValue).
                   oError:Add('errorNum':u, 0).
                   iStart = 2.
                end.
            end.
            if(type-of(poError,ApplicationError)) then
            do iLoop = iStart to iMax:
               oErrorList:Add(cast(poError,ApplicationError):GetError(iLoop)).   
            end.
            else do iLoop = iStart to iMax:
                assign oError = new JsonObject().  
                oErrorList:Add(oError).
                
                oError:Add('errorMsg':u, poError:GetMessage(iLoop)).
                oError:Add('errorNum':u, poError:GetMessageNum(iLoop)).
            end.
            oResponse:Add('errors':u, oErrorList).
/*        end.*/
        case true:
            // single error? add an simple JSON object
            when not valid-object(this-object:Entity) then
                assign this-object:Entity = oResponse.
            
            // if there's already an array, add to it                
            when type-of(this-object:Entity, JsonArray) then
                cast(this-object:Entity, JsonArray):Add(oResponse).
            
            // if there's a single json object, switch to an array
            when type-of(this-object:Entity, JsonObject) then
            do:
                assign oErrorList = new JsonArray().
                oErrorList:Add(cast(this-object:Entity, JsonObject)).
                oErrorList:Add(oResponse).
                
                assign this-object:Entity = oErrorList.
            end.
        end case.
        
        if session:debug-alert then
            oResponse:Add('type':u, poError:GetClass():TypeName).
            
        if poError:CallStack ne ? then
        do:
            assign oErrorList = new JsonArray()
                   iMax       = num-entries(poError:CallStack, StringConstant:LF)
                   .
            oResponse:Add('stack':u, oErrorList).
            do iLoop = 1 to iMax:
                oErrorList:Add(entry(iLoop, poError:CallStack, StringConstant:LF)).
            end.
        end.
        
        assign oProp = poError:GetClass():GetProperty('InnerError':u).
        if valid-object(oProp) and
           oProp:DataType eq DataType:Object and
           Progress.Lang.Class:GetClass(oProp:DataTypeName):IsA(get-class(Progress.Lang.Error)) then
        do:           
            assign oInner = oProp:Get(poError).
            if valid-object(oInner) then
                this-object:WriteError(oInner).
        end.
        if not valid-object(oInner) then
        do:
            assign oProp = poError:GetClass():GetProperty('InnerStop':u).
            if  valid-object(oProp) 
            and oProp:DataType eq DataType:Object 
            and Progress.Lang.Class:GetClass(oProp:DataTypeName):IsA(get-class(Progress.Lang.Stop)) then
            do: 
                assign oInnerStop = oProp:Get(poError).
                if valid-object(oInnerStop) then
                    this-object:WriteStop(oInnerStop).
            
            end.    
        end.
    end method.
    
end class.
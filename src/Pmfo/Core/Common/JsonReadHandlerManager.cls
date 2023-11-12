 
 /*------------------------------------------------------------------------
    File        : JsonReadHandlerManager
    Purpose     : Manage multiple IJsonReadHandlers
                    
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : 3 Feb 2021
    Notes       :  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Common.JsonReadHandlerManager implements IJsonReadHandler: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

      
    define protected property HandlerMap as IMap no-undo get. protected set. 
    
    define protected property DefaultReadHandler as IJsonReadHandler no-undo get. protected set. 
    
    define protected variable CurrentReader as IJsonReadHandler no-undo.     
    
    constructor public JsonReadHandlerManager (pHandlerMap as IMap ):
        super ().
        HandlerMap = pHandlerMap.
    end constructor.
    
    constructor public JsonReadHandlerManager (pDefaulHandler as IJsonReadHandler, pcEntityName as character, poHandler as IJsonReadHandler ):
        super ().
        DefaultReadHandler = pDefaulHandler.
        HandlerMap = new Map().
        HandlerMap:Put(new String(pcEntityName),poHandler).  
    end constructor.
    
    constructor public JsonReadHandlerManager (pcEntityNames as character extent, poHandlers as IJsonReadHandler extent ):
        define variable i as integer no-undo.
        super ().
        
        HandlerMap = new Map().
        do i = 1 to extent(pcEntityNames):
            HandlerMap:Put(new String(pcEntityNames[i]),poHandlers[i]).  
        end.    
    end constructor.
    
    method public logical CheckReadOnlyAndPrepareBuffer( phBuffer as handle, pState as RowStateEnum).
       CurrentReader = GetHandler(phBuffer).
       if not valid-object (CurrentReader) then 
           undo, throw new IllegalStateError(subst("The JsonReadHandleManager cannot find a handler for entity '&1'",phbuffer:table-handle:serialize-name)).
            
       return CurrentReader:CheckReadOnlyAndPrepareBuffer(phBuffer,pState).
    end method.
    
    method protected IJsonReadHandler GetHandler(phBuffer as handle):
        define variable oReader as IJsonReadHandler no-undo. 
        oReader = cast(HandlerMap:Get(new String(phbuffer:table-handle:serialize-name)),IJsonReadHandler).
        if not valid-object(oReader) then 
        do: 
            if valid-object(DefaultReadHandler) then 
                return DefaultReadHandler.
            if valid-handle(phbuffer:parent-relation) then 
                oReader = GetHandler(phbuffer:parent-relation:parent-buffer).    
        end.
        return oReader.
    end.
     
    method public logical ReadField( pjson as JsonObject,phfld as handle):
        return CurrentReader:ReadField(pjson, phfld). 
    end method.   
    
    // no default behavior
    method public void HandleFieldNotInJson( pjson as JsonObject, phfld as handle).
        CurrentReader:HandleFieldNotInJson(pjson, phfld). 
    end method.
     
end class.
 
 /*------------------------------------------------------------------------
    File        : GetDataQuestions
    Purpose     : Collection of questions for GetData request  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jul 03 09:13:39 EDT 2023
    Notes       : property on IGetdataRequest and IGetDataResponse 
                  transferred from here to the response at end of request  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.Common.IGetDataMessage from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.

class Pmfo.Core.Common.GetDataMessages implements IGetDataMessages: 
    
    define private variable mHasCollection as logical no-undo.
         
    define protected property Messages as IList  no-undo  
        get():
            if not valid-object(Messages) then 
                Messages = new List().
            return Messages.    
        end.     
        protected set(pMessages as IList):
            if mHasCollection and Messages:Size > 0 then   
                 undo, throw new IllegalOperationError("Messages is already set and has data"). 
            Messages = pMessages.
            mHasCollection = true. // 
        end.
         
    define public property Size as integer init ? no-undo 
        get():
            // avid lazy load until really needed
            if mHasCollection = false then 
                return 0.
            else 
                return Messages:size.
        end.
     
 

    constructor public GetDataMessages (  ):
        super ().
    end constructor.
    
    method public logical Add( input poMessage as IGetDataMessage ):
        
       // if Questions:Contains(poQuestion) then
         //   undo, throw new UnsupportedOperationError(subst("Question '&1' is already added to request",
           //                                                      poQuestion:key
             //                                              )
               //                                       ).
                             
        return Messages:Add(poMessage).
    end method.
     
    method public logical AddAll( input poMessages as IGetDataMessages):
        define variable oGetDataMessages as IGetDataMessage extent no-undo.
        define variable lAdded            as logical no-undo.
        define variable i                 as integer no-undo.
        
        oGetDataMessages = poMessages:ToArray().
        do i = 1 to extent(oGetDataMessages):
            if this-object:Add(oGetDataMessages[i]) then
                lAdded = true.
        end.
        
        return lAdded.
        
    end method.
        
    method public void Clear():
        if mHasCollection then 
        do:
            Messages:Clear().
        end.
        Messages = ?.
    end method.    
    
    method public JsonArray ToJsonArray(  ):
        define variable oIter            as IIterator no-undo.
        define variable oGetDataMessage  as IGetDataMessage no-undo.
        define variable oJsonArray       as JsonArray no-undo.
        
        oJsonArray =  new JsonArray(). 
        if mHasCollection then
        do: 
            oIter = Messages:Iterator(). 
            do while oIter:HasNext():
                oGetDataMessage = cast(oIter:Next(),IGetDataMessage).
                oJsonArray:Add(oGetDataMessage:ToJson()).
            end.
            
        end. 
        return oJsonArray.
        
    end method.
    
     method public IGetDataMessage extent ToArray(  ):
        define variable oGetDataMessages as IGetDataMessage extent no-undo.
        
        if mHasCollection then
            oGetDataMessages = cast(Messages:ToArray(),IGetDataMessage).
        return oGetDataMessages.
    end method.
end class.
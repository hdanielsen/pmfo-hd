 
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
using Pmfo.Core.Common.IGetDataQuestion from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.

class Pmfo.Core.Common.GetDataQuestions implements IGetDataQuestions: 
    
    
    define private variable mQuestions as IList no-undo.
   
    define protected property HasList as logical no-undo 
        get(): 
            return valid-object(mQuestions).
        end. 
    
    define protected property Questions as IList  no-undo  
        get():
            if not valid-object(mQuestions) then 
                 mQuestions = new List().
            return mQuestions.    
        end.     
        protected set(pquestions as IList):
            if HasList and mQuestions:size > 0 then 
                 undo, throw new IllegalOperationError("Questions is already set and has data"). 
            mQuestions = pquestions.
        end. 
    
    define public property Size as integer no-undo 
        get():
            if HasList then 
                return Questions:size.
            else 
                return 0.     
        end.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public GetDataQuestions (  ):
        super ().
    end constructor.
    
    method public logical Add( input poQuestion as IGetDataQuestion ):
        
       // if Questions:Contains(poQuestion) then
         //   undo, throw new UnsupportedOperationError(subst("Question '&1' is already added to request",
           //                                                      poQuestion:key
             //                                              )
               //                                       ).
                             
        return Questions:Add(poQuestion).
    end method.
     
    method public logical AddAll( input poQuestions as IGetDataQuestions):
        define variable oGetDataQuestions as IGetDataQuestion extent no-undo.
        define variable lAdded            as logical no-undo.
        define variable i                 as integer no-undo.
        
        oGetDataQuestions = poQuestions:ToArray().
        do i = 1 to extent(oGetDataQuestions):
            if this-object:Add(oGetDataQuestions[i]) then
                lAdded = true.
        end.
        
        return lAdded.
        
    end method.
        
    method public void Clear():
        if HasList then 
        do:
            Questions:Clear().
        end.
        Questions = ?.
    end method.    
    
    method public JsonArray ToJsonArray(  ):
        define variable oIter            as IIterator no-undo.
        define variable oGetDataQuestion as IGetDataQuestion no-undo.
        define variable oJsonArray       as JsonArray no-undo.
        
        oJsonArray =  new JsonArray(). 
        if HasList then
        do: 
            oIter = Questions:Iterator(). 
            do while oIter:HasNext():
                oGetDataQuestion = cast(oIter:Next(),IGetDataQuestion).
                oJsonArray:Add(oGetDataQuestion:ToJson()).
            end.
            
        end. 
        return oJsonArray.
        
    end method.
    
     method public IGetDataQuestion extent ToArray(  ):
        define variable oGetDataQuestions as IGetDataQuestion extent no-undo.
        
        if HasList then
            oGetDataQuestions = cast(Questions:ToArray(),IGetDataQuestion).
        return oGetDataQuestions.
    end method.
end class.
 
 /*------------------------------------------------------------------------
    File        : MemoryInstanceBE
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 10/07/2024 -moved up from application    
    Notes       :  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.ClientBusinessEntity from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Common.IResource from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.Common.IMethod from propath.
using OpenEdge.Core.WidgetHandle from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IMap from propath.

class Pmfo.Core.BusinessLogic.MemoryInstanceBE inherits ClientBusinessEntity: 
    {Pmfo/Core/schema/memoryInstance.i}
        
    define dataset dsMemoryInstance for ttMemoryInstance.
        
    constructor public MemoryInstanceBE (   ):
        super (dataset dsMemoryInstance:handle).
    end constructor.

    method public override int64 CreateSourceRecords(output table-handle phTable):
        return CreateMemoryInstances(output table-handle phTable by-reference).
    end method.
         
    method protected int64 CreateMemoryInstances(output table ttMemoryInstance).
        define variable oObject as Object no-undo.
        define variable iCount as integer no-undo.
        define variable iSessionId as integer no-undo.
        
        iSessionId =  session:current-request-info:SessionID no-error.
        oObject = session:first-object.
        do while valid-object(oObject):
            create ttMemoryInstance.
            assign 
                ttMemoryInstance.ref        = int64(oObject)
                ttMemoryInstance.type       = oObject:GetClass():Typename
                ttMemoryInstance.toString   = oObject:ToString()
                ttMemoryInstance.hasStatics = oObject:GetClass():HasStatics()
                ttMemoryInstance.sessionId  = iSessionId
                iCount = iCount + 1
                ttMemoryInstance.zz_seq    = iCount
                .
            if type-of(oObject,WidgetHandle) then     
            do: 
                define variable h as handle no-undo.
                h = handle(ttMemoryInstance.toString) no-error.
                if valid-handle(h) then 
                    ttMemoryInstance.info = subst("&1 &2",h:type,h:name) .
                else   
                   ttMemoryInstance.info = "[invalid handle]".
            end.         
            else if type-of(oObject,ICollection) then 
            do:    
                ttMemoryInstance.info = subst("Size &1",cast(oObject,ICollection):Size).
                ttMemoryInstance.info = ttMemoryInstance.info + " " + cast(oObject,ICollection):Iterator():Next():GetClass():TypeName no-error.
            end.     
            else if type-of(oObject,IMap) then
            do:     
                ttMemoryInstance.info  = subst("Size &1",cast(oObject,IMap):Size).
                if cast(oObject,IMap):Size > 0 then
                    ttMemoryInstance.info = subst("&1 &2 - &3",
                                                  ttMemoryInstance.info,   
                                                  cast(oObject,IMap):KeySet:Iterator():Next():GetClass():TypeName,
                                                  cast(oObject,IMap):Values:Iterator():Next():GetClass():TypeName
                                                 )       
                                            no-error.
            end.
            else if type-of(oObject,IResource) then     
                ttMemoryInstance.publicName = cast(oObject,IResource):Resource. 
            else if type-of(oObject,IDataSource) then     
                ttMemoryInstance.publicName = cast(oObject,IDataSource):EntityName. 
            else if type-of(oObject,IMethod) then     
                ttMemoryInstance.publicName = cast(oObject,IMethod):Name. 
            
            oObject = oObject:next-sibling.
        end.
        return iCount.
    end method.
    
     
end class.
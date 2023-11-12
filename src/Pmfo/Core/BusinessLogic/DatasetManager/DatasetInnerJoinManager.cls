 
 /*------------------------------------------------------------------------
    File        : DatasetUpdateRowWalker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.Update.AbstractUpdateTargetManager from propath.
using Pmfo.Core.DataLayer.Update.IDatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.

class Pmfo.Core.BusinessLogic.DatasetManager.DatasetInnerJoinManager  : 
    define property SeqFieldName as character no-undo init "zz_seq" get. 
     /* Always False for nested update operations */
    define public property ShallowDelete as logical no-undo get. 
    
    constructor public DatasetInnerJoinManager ( ):
        super ( ).
    end constructor.
     
    method public void ProcessInnerJoin(phBuffer as handle, pcInnerJoins as character, poSort as IQuerySortEntry extent):
        define variable hQuery          as handle no-undo.
        define variable cSortExpression as character no-undo.
        define variable i               as integer no-undo.
        define variable iCount          as integer no-undo.
        define variable hSequence       as handle no-undo.
        
        hSequence = phbuffer:buffer-field(SeqFieldName) no-error.
        
        do i = 1 to extent(poSort):
            cSortExpression = cSortExpression 
                        + (if cSortExpression = "" then "" else " ")
                        + poSort[i]:ToString().
        end.
        
        create query hQuery.
        
        hQuery:add-buffer(phBuffer).
        hQuery:query-prepare(trim(subst("preselect each &1 &2",phBuffer:name,cSortExpression))).
        
        hQuery:query-open().
        hQuery:get-first.
        do while phBuffer:avail:
            if pcInnerJoins > "" then
            do:
                if HasEmptyInnerJoinChildren(phBuffer,pcInnerJoins) then   
                     phBuffer:buffer-delete().
            end.     
            if valid-handle(hSequence) then
            do:
                iCount = iCount + 1.
                hSequence:buffer-value = iCount.
            end.     
            hQuery:get-next. 
        end.
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.
        end.
        
    end method.
    
    method protected logical HasEmptyInnerJoinRows(phRelation as handle,pcInnerJoins as character):
        
        define variable hQuery  as handle no-undo.
        define variable cJoin as character no-undo.
        define variable hChildbuffer as handle no-undo.
        
        hChildbuffer = phRelation:child-buffer.
        if lookup(hChildBuffer:table-handle:name,pcinnerJoins) = 0 then
            return false.
            
        cJoin = JoinExpression(phRelation).
        
        if not hChildbuffer:find-first(cJoin) then 
            return true.
        
        create query hQuery.
        hQuery:add-buffer(hChildbuffer).
        
        // preselect - see comment in DatasetUpdateManager ProcessRows  
        hQuery:query-prepare(right-trim(subst("for each &1 &2",hChildbuffer:name,cJoin))).
        hQuery:query-open().
        hQuery:get-first.
        do while hChildbuffer:avail:
            if HasEmptyInnerJoinChildren(hChildbuffer,pcInnerJoins) then 
                return true.
            hQuery:get-next. 
        end. 
        return false.
        finally:
           if valid-handle(hQuery) then
                delete object hQuery.
        end.    
    end method.
    
    method protected character JoinExpression(phRelation as handle ):
        return BufferUtil:GetJoinExpression(phRelation,false).
    end method.   
    
   
     method protected logical HasEmptyInnerJoinChildren(phBuffer as handle, pcInnerJoins as character):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if HasEmptyInnerJoinRows(hRelation,pcInnerJoins) then 
                return true.
        end.     
        return false.
    end method.
end class.
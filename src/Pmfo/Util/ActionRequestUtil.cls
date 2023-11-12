 
 /*------------------------------------------------------------------------
    File        : ActionRequestUtil 
    Purpose     : May need a more precise name (or more generic), but not sure what the exactg use is  
                  The main gaol is to create request for multiple records and then validate that all records of the OR expressions 
                  used for this are present/found    
                - Was added to support passing a single text to apply to many parents
                  Since the note-id is not exposed on the parent it does not need to be conflct checked
                  and no refresh is needed
                - The following seems to be the criteria where this may be useful 
                  - Existing parents - 
                  - No update of the parent - seem to be main criteria as oitherwise the client should send all data
                  - Need other info about parent, mnay even update parent data that i sno exposed - i.e note-id
                  - Multiple different parents - like note, image  
                  - Add data to multiple records of one oe the types 
                  - One single action with data nad.properations for many
                -      
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 05 09:36:19 EDT 2022
    Notes       : Typically used to validate that all input is valid for actions where 
                  keys are provided in the body or parameter and a GetTableRequest is 
                  used to get all data from server before the actual execution
                  of the action   
                - Also supports building the request from temp-table records with only keys
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Util.ActionRequestUtil: 
    
    method public void ValidateAllDataInBuffer(pTableRequest as IGetDataTableRequest, phBuffer as handle, pcKeys as character):
        define variable oQueryEntries as IQueryEntry extent no-undo.
        define variable cKeys         as character extent no-undo.
        define variable hField        as handle no-undo.
        define variable hFields        as handle extent no-undo.
        define variable i             as integer no-undo.
        Assert:NotNull(phBuffer,"Buffer").
        Assert:IsTrue(phBuffer:type = "Buffer","Handle Type is 'Buffer'").
         
        if num-entries(pcKeys) > 1 then 
        do:
            cKeys = Array:Split(pcKeys).
            extent(hFields) = extent(cKeys).
            oQueryEntries = pTableRequest:GetQueryEntries(cKeys,QueryOperatorEnum:Eq).
            if extent(oQueryentries) > 1 then // TODO throw a not implemented if this is an extent of predicates with EQ
                undo, throw new UnsupportedOperationError(subst("Query &1 is too complex for buffer key validation",pTableRequest:QueryDefinition)).
         
            do i = 1 to extent(ckeys):
                hFields[i] = phBuffer:buffer-field(cKeys[i]).
            end.    
            ValidateAllDataInBuffer(oQueryEntries[1], phBuffer, hFields).
        end.
        else  
        do:
            oQueryEntries = pTableRequest:GetQueryEntries(pcKeys,QueryOperatorEnum:InList).
            if extent(oQueryentries) > 1 then // TODO throw a not implemented if this is an extent of predicates with EQ
                undo, throw new UnsupportedOperationError(subst("Query &1 is too complex for buffer key validation",pTableRequest:QueryDefinition)).
            hField = phBuffer:buffer-field(pcKeys).
            ValidateAllDataInBuffer(oQueryEntries[1], phBuffer, hField).
        end. 
    end method.    
    
    method protected void ValidateAllDataInBuffer(pQueryEntry as IQueryEntry, phBuffer as handle, phFLd as handle):
        define variable i as integer no-undo.
        define variable cFind as character no-undo.
        define variable cValues as character extent no-undo. 
        
        if type-of(pQueryEntry,IQueryPredicate) then 
        do:
            if cast(pQueryEntry,IQueryPredicate):Operator = QueryOperatorEnum:InList then             
            do:
                cValues = cast(pQueryEntry,IQueryPredicate):Stringvalues.
              
                do i = 1 to extent(cValues)
                on error undo, throw:
                    case phFLd:data-type:
                         when "character" then 
                             cFind = subst("where &1 = '&2'",phFLd:name, cValues[i]).
                         otherwise    
                             cFind = subst("where &1 = &2",phFLd:name, cValues[i]).
                    end case.     
                    phBuffer:find-unique (cfind).
                    catch e as Progress.Lang.Error :
                        if e:GetMessageNum(1) = 138 then 
                            // inner error is not helpful here (?)
                            undo, throw new NotFoundError(subst("&1 &2",BufferUtil:GetRowName(phBuffer,true),cFind)).
                        else 
                            undo, throw e.
                    end catch.
                     
                end.
           end.
           else do:
               // throw not implemented as this can be possibly be supported (although probably not reaching here though since there would be many
               // NOTE of this happens then CreateRequestForRows was likely not used to create the request  
               undo, throw new NotImplementedError(subst("Predicate '&1' operator '&2' in buffer key validation. Use InList for single key.", 
                                                                cast(pQueryEntry,IQueryPredicate),
                                                                cast(pQueryEntry,IQueryPredicate):Operator 
                                                                )
                                                          ). 
           end.    
       end.
       else do:     
           if type-of(pQueryEntry,Ccs.BusinessLogic.IQueryPredicate) then 
               undo, throw new UnsupportedOperationError(subst("Predicate &1 is type &2 and is unsupported in  buffer key validation",
                                                                pQueryEntry,
                                                                pQueryEntry:getClass():TypeName
                                                              )
              ).
           else 
               undo, throw new UnsupportedOperationError(subst("Query &1 is too complex for buffer key validation",pQueryEntry)).
        end.
        
    end method.    
    
    method protected void ValidateAllDataInBuffer(pQueryEntry as IQueryEntry, phBuffer as handle, phFLds as handle extent):
        define variable oEntries as IQueryEntry no-undo extent.
        define variable i as integer no-undo.
        define variable oPredicate as IQueryPredicate no-undo.
        define variable cFind as character no-undo.
        define variable hQuery as handle no-undo.
        define variable oOrEntries as character no-undo.
        define variable IORStart as integer no-undo.
        define variable cValues as character extent no-undo.
        define variable oTopQueryGroup as QueryGroup no-undo.
        extent(cValues) = extent(phFLds).
        
        if type-of(pQueryEntry,IQueryGroup) then
        do:
            oEntries = cast(pQueryEntry,IQueryGroup):Entries.
            // The top groups does not have or and will appear as indivdual predicates
            oTopQueryGroup = new QueryGroup().
            if type-of(oEntries[i],Ccs.BusinessLogic.IQueryPredicate) then 
            do:
                do i = 1 to extent(phFlds) :
                    if type-of(oEntries[i],IQueryPredicate) then 
                    do:
                        oTopQueryGroup:AddEntry(oEntries[i]). 
                    end.
                    else do:
                       if type-of(oEntries[i],Ccs.BusinessLogic.IQueryPredicate) then 
                           undo, throw new UnsupportedOperationError(subst("Predicate &1 is type &2 and is unsupported in buffer key validation",
                                                                            oEntries[i],
                                                                            oEntries[i]:getClass():TypeName
                                                                          )
                                                                     ).
                       else 
                           undo, throw new UnsupportedOperationError(subst("Query &1 is too complex for buffer key validation",pQueryEntry)).
                    end. 
                end.
                iOrStart = extent(phFlds). 
                FindRow(phbuffer,oTopQueryGroup).
            end. 
            IOrStart = iOrStart + 1.                 
                
            do i = iOrStart to extent(oEntries) :  
                if i > 1  and oEntries[i]:Join <> JoinEnum:Or then
                    undo, throw new UnsupportedOperationError(subst("Query Entry '&1' in the group is not joined with an OR. The expression '&2' is not valid for buffer key validation",
                                                                    oEntries[i]:getClass():TypeName,
                                                                    pQueryEntry
                                                              )
                                                          ).
                if type-of(oEntries[i],Pmfo.Core.BusinessLogic.IQueryEntry) then                        
                    FindRow(phbuffer,cast(oEntries[i],Pmfo.Core.BusinessLogic.IQueryEntry)).
                else
                    undo, throw new UnsupportedOperationError(subst("Query Group &1 is type &2 and is unsupported in buffer key validation",
                                                                        oEntries[i],
                                                                        oEntries[i]:getClass():TypeName
                                                                      )
                                                                 ).                                                  
            end.
        end.   
        else 
            undo, throw new IllegalArgumentError(subst("QueryEntrty of type  &1 passed to multi key validation",pQueryEntry:GetClass():TypeName)).    
        
    end method.    
    
    
    method protected void FindRow(phBuffer as handle, pQueryEntry as Pmfo.Core.BusinessLogic.IQueryEntry):
        phBuffer:find-unique ("where " + pQueryEntry:ToString()).
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 138 then 
                        // inner error is not helpful here (?)
                undo, throw new NotFoundError(subst("&1 &2",BufferUtil:GetRowName(phBuffer,true),"where " + pQueryEntry:ToString())).
            else 
                undo, throw e.
         end catch.
    end method.     
    
    method protected handle CreateQuery(phBuffer as handle):
        define variable hQuery as handle no-undo.
        create query hQuery.
        Assert:NotNull(phBuffer,"Buffer").
        Assert:IsTrue(phBuffer:type = "Buffer","Handle Type is 'Buffer'").
         
        hQuery:add-buffer(phBuffer).
        hQuery:query-prepare(subst("for each &1",phBuffer:name)).
        return hQuery.
    end method.
    
    method public IGetDataTableRequest CreateRequestForRows(phbuffer as handle):
        define variable cKeys as character no-undo.
        
        cKeys = ServiceInfo:GetPublicKeys(phBuffer).
        return CreateRequestForRows(phBuffer,cKeys).
        
    end method.
    
    method public IGetDataTableRequest CreateRequestForRows(phbuffer as handle, pcKeys as character):
        return CreateRequestForRows(phBuffer:table-handle:name, phbuffer, pcKeys).
    end method.
    
    method public IGetDataTableRequest CreateRequestForRows(pcTable as character, phbuffer as handle, pcKeys as character):
         define variable hFlds as handle extent no-undo.
         define variable i  as integer no-undo.
         define variable hQuery as handle no-undo.
         define variable oRequest as IGetDataTableRequest no-undo.
         define variable hFld as handle no-undo.
         define variable oQueryGroup as IQueryGroup no-undo.
         define variable oQueryPredicates as IQueryPredicate extent no-undo.
         define variable lAddedTogroup as logical no-undo.
         define variable cValues as character extent no-undo.
         define variable lMultiKey  as logical no-undo.
         
         oRequest = new GetDataTableRequest(pcTable).
         
         // use variable since it is a bit confusing that values extent is used for single key  
         lMultiKey = num-entries(pcKeys) > 1.
         
         if lMultiKey then 
         do:
             extent(hFLds) = num-entries(pcKeys).
             extent(oQueryPredicates) = extent(hFlds).
             do i = 1 to extent(hFLds):
                 hFLds[i] = phBuffer:buffer-field(entry(i,pckeys)). 
             end.
                 
         end.         
         else do:
             hFld = phBuffer:buffer-field(pckeys).
         end.
         
         hQuery = CreateQuery(phBuffer).
         hQuery:query-open().
         hQuery:get-first.
        
         do while hQuery:get-buffer-handle (1):avail:  
             if extent(hFlds) <> ? then
             do:
                  do i = 1 to extent(hFlds):
                      oQueryPredicates[i] = new QueryPredicate(hFlds[i]:name,QueryOperatorEnum:Eq,hFlds[i]:buffer-value).
                  end.
                  oQueryGroup = new QueryGroup(if lAddedTogroup then JoinEnum:Or  else JoinEnum:None,oQueryPredicates).
                  lAddedTogroup = true.
                  oRequest:AddQueryEntry(oQueryGroup). 
             end.        
             else do: 
                 extent(cValues) = (if extent(cValues) = ? then 0 else extent(cValues)) + 1.
                 cValues[extent(cValues)] = hFLd:buffer-value.
             end.
             hQuery:get-next ().
         end.
         if lMultiKey = false then 
         do:
             oRequest:AddQueryEntry(new QueryPredicate(hFld:name,QueryOperatorEnum:Inlist,cValues)). 
         end.
       
         return oRequest.
         finally:
             if valid-handle(hQuery) then
                delete object hQuery.         
         end finally. 
          
    end method.  
    
    method protected void ThrowUnsupportedOperator():  
    end method.

end class.
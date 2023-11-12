 
 /*------------------------------------------------------------------------
    File        : QueryRequestBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 02 12:18:57 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Core.BusinessLogic.Map.EntityFieldMapHolder from propath.
using Pmfo.Core.BusinessLogic.Parser.FieldExpressionParser from propath.
using Pmfo.Core.BusinessLogic.Parser.FilterParser from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWildcardConverter from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.DataRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
 

class Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder inherits DataRequestBuilder: 
    
            
    define protected property FieldMapHolder as EntityFieldMapHolder no-undo 
        get():
            if not valid-object(FieldMapholder) then 
                FieldMapholder = new EntityFieldMapholder().
            return FieldMapholder.     
        end. 
        protected set.
    
    constructor public QueryRequestBuilder ( ):
        super ().
    end constructor.
 
    method protected override void  ProcessMainTableRequest(pRequests        as ICollection, 
                                                            pClientRequest   as IClientGetRequest,
                                                            pTableRequest    as IGetDataTableRequest, 
                                                            phBuffer         as handle,
                                                            piCollection     as integer,
                                                            poRequestType    as RequestTypeEnum):
         
        define variable cOption        as character           no-undo.
        define variable iSkip          as int64               no-undo.
        define variable iNumRecords    as int64               no-undo.
        define variable cFilter        as character           no-undo.
        define variable cSort          as character           no-undo. 
        define variable cApplyFilter   as character no-undo.
        define variable cPagingContext as character           no-undo.
       
        define variable oFilterParser    as FilterParser        no-undo.
        define variable lCount           as logical             no-undo.    
        define variable oParamEntry      as IQueryEntry         no-undo.
        define variable oTableRequest    as GetDataTableRequest no-undo. 
        define variable oQueryDefinition as IQueryDefinition    no-undo.
        define variable oApplyQueryDefinition as IQueryDefinition    no-undo.
        define variable oFieldExpressionParser as FieldExpressionParser no-undo.
        oTableRequest = cast(pTableRequest,GetDataTableRequest). 
        if RequestTypeEnum:SingleRow:Equals(poRequestType) = false then
        do:
            iSkip = pClientRequest:Skip.
            if iSkip > 0 then
                oTableRequest:Skip = iSkip.
            lCount = pClientRequest:Count .
            if lCount then
                pTableRequest:Count = lCount.
                
            cPagingContext = pClientRequest:PagingContext.
          
            if cPagingContext > "" then 
            do:
                if iSkip > 0 then 
                    undo, throw new BadRequestError(subst("Conflicting query properties '&1' and '&2'",pClientRequest:SkipProp,pClientRequest:PagingContextProp)).
                oFieldExpressionParser = new FieldExpressionParser(FieldMapHolder).
                oTableRequest:PagingContextEntry = oFieldExpressionParser:GetKeyEntry(cPagingContext, phBuffer).    
            end.
            
            iNumRecords = pClientRequest:NumRecords.  
            if iNumRecords > 0 then 
                oTableRequest:NumRecords = iNumRecords.
            
            cFilter = pClientRequest:Filter.  
            cSort   = pClientRequest:Sort.  
            cApplyFilter = pClientRequest:ApplyFilter.
            if cFilter > "" or cSort > "" or cApplyFilter > "" then 
            do:
                // the field map holder is empty at this point, it is passed in for reuse here latere
                // pass the expanded relations to validate qualified references   
                oFilterParser = new FilterParser(FieldMapHolder,pClientRequest:ExpandedRelations).
                if cFilter > "" or cSort > "" then 
                do:
                    oQueryDefinition = oFilterParser:GetQueryDefinition(phBuffer,cFilter, cSort).  
                    if oFilterParser:HasWarnings then 
                    do: 
                        // hackety hack
                        Warnings = oFilterParser:Warnings. 
                    end.    
                    
                    if cFilter > "" and pclientRequest:Wildcards then 
                    do:
                        new QueryWildcardConverter():Traverse(oQueryDefinition). 
                    end.     
                    
                    // for a function we may have used keywhere to build a filter already so add it to the new definition 
                    if valid-object(oTableRequest:QueryDefinition) and valid-object(oTableRequest:QueryDefinition:QuerySelection) then 
                        oQueryDefinition:AddEntry(oTableRequest:QueryDefinition:QuerySelection).
                     oTableRequest:QueryDefinition = oQueryDefinition.
                end.
                if cApplyFilter > "" then 
                do:
                    oApplyQueryDefinition = oFilterParser:GetQueryDefinition(phBuffer,cApplyFilter,"").  
                    if oFilterParser:HasWarnings then 
                    do: 
                        // hackety hack
                        Warnings = oFilterParser:Warnings. 
                    end.    
                    if valid-object(oTableRequest:QueryDefinition) and valid-object(oTableRequest:QueryDefinition:QuerySelection) then 
                        oApplyQueryDefinition:AddEntry(oTableRequest:QueryDefinition:QuerySelection).
                     oTableRequest:QueryDefinition = oApplyQueryDefinition.
                end.
                
            end.  
            // ClientExpand is used to exclude the specifically requested expands from the qualified filter refs in
            // GetDataTableRequest:GetInnerjoins output parameter for server joins   
            if pClientRequest:Expand > "" then 
                oTableRequest:ClientExpands = GetJoinTables(phBuffer,
                                                            pClientRequest:Expand,
                                                            true). //ignore errors since expond may be completely managed in BE 
            // we will merge these innerjoins in GetDataTableRequest:GetInnerjoins
            if pClientRequest:ExpandInnerJoins > "" then 
            do on error undo, throw: 
                oTableRequest:InnerJoinExpands = GetJoinTables(phBuffer,
                                                               pClientRequest:ExpandInnerJoins,
                                                               false). // don't ignore error 
                catch e as IllegalArgumentError :
                     undo, throw new IllegalStateError("One or more of the expand(s) with innerjoin=true is invalid",e).   
                end catch. 
            end.                                                     
            if pClientRequest:ApplyInnerJoins > "" then
            do on error undo, throw: 
                oTableRequest:InnerJoinAggregates = GetJoinTables(phBuffer,
                                                                  pClientRequest:ApplyInnerJoins,
                                                                  false).  // don't ignore error
                catch e as IllegalArgumentError :
                     undo, throw new IllegalStateError("One or more of the qualified aggregates is invalid",e).   
                end catch.                                                   
            end.                                                        
        end.
        // if select get the unqualified fields  for the main table from the option
        if pClientRequest:Select > "" then
        do:
            oTableRequest:SelectFields = GetTTFieldsFromSelect(phBuffer,pClientRequest:Select,"").
        end.
        // skip reposition tables for find request
        AddTableChildrenToRequestTree(prequests, pClientRequest, phBuffer, "", oTableRequest:FindUnique).
    end method.
    
    method protected character GetJoinTables(phbuffer as handle,pcInnerJoins as character, plIgnoreErrors as logical):
        define variable i                as integer   no-undo.
        define variable cTables          as character no-undo.
        define variable hExpandBuffer    as handle no-undo.
        // TODO - somewhat inefficient perhaps -  
        do i = 1 to num-entries(pcInnerJoins):
            hExpandBuffer = BufferUtil:GetBufferFromUrlQualifier(phbuffer, entry(i,pcInnerJoins)).
            if valid-handle(hExpandBuffer) then 
            do:
                cTables = cTables 
                        + (if i = 1 then "" else ",")
                        + BufferUtil:GetBufferFromUrlQualifier(phbuffer, entry(i,pcInnerJoins)):table-handle:name.
            end.
            else if not plIgnoreErrors then 
                undo, throw new IllegalArgumentError(subst("Entry '&1' in Join Tables '&2' is not validd",entry(i,pcInnerJoins),pcInnerJoins)).          
        end.
        return cTables.
    end method.    
    
    method protected character GetTTFieldsFromSelect(phBuffer as handle, pcFields as character, pcRelativePath as character):
        define variable i             as integer no-undo.
        define variable cField        as character no-undo.
        define variable cTTField      as character no-undo.
        define variable cTTFieldList  as character no-undo.
        define variable cdlm          as character no-undo.
        define variable cFieldMap     as character no-undo.
        define variable cQualifier    as character no-undo.
        define variable hQualBuffer   as handle no-undo.
        define variable hTopBuffer    as handle no-undo.
        
        if pcRelativePath > "" then 
        do: 
             hTopBuffer = phBuffer:dataset:get-buffer-handle(1).
             hQualBuffer = BufferUtil:GetBufferFromUrlQualifier(hTopBuffer, pcRelativePath).
             cFieldMap   = FieldMapHolder:GetFieldMap(hQualBuffer).
             // as of current the select fields only have the last entry as qualifier  
             cQualifier  = entry(num-entries(pcRelativePath,"/"),pcRelativePath,"/").
        end.    
        else       
            cFieldMap   = FieldMapHolder:GetFieldMap(phBuffer).
            
        do i = 1 to num-entries(pcFields):
            cField = entry(i,pcfields).
            if (cQualifier = "" and num-entries(cField,".") = 1)
            or (cQualifier > "" and cField begins cQualifier + ".") then 
            do:
                if cQualifier > "" then
                    cField = entry(2,cField,".").
                assign
                    cTTField     = StringUtil:GetUseName(cField, cFieldMap).     
                    cTTFieldList = cTTFieldList
                                 + cDlm  
                                 + cTTField.
                    CDlm  = ",".      
            end.    
        end.    
        return cTTFieldList.
    end method.    
    
    /**
    Process dataset is used when there are more thna one top-table  
    **/
    method protected override void ProcessDatasetRequest(input pRequests as ICollection, input phDataset as handle):
        define variable i as integer no-undo.
        do i = 1 to phdataset:num-top-buffers:
            AddTableRequestTree(pRequests,?,phDataset:get-top-buffer(i)). 
        end.       
    end method. 
    
    /**
    Add a buffer to the request - called for children of main table or when there is no main table
    pClientRequest is only passed from main table 
    **/
    method protected void AddTableRequestTree( prequests as ICollection, pClientRequest as IClientGetRequest, phBuffer as handle):
        this-object:AddTableRequestTree(prequests, pClientRequest, phBuffer, phBuffer:serialize-name, false). 
    end method.
    
    method protected void AddTableRequestTree( prequests as ICollection, pClientRequest as IClientGetRequest, phBuffer as handle , pcRelativePath as character):
        this-object:AddTableRequestTree(prequests, pClientRequest, phBuffer, pcRelativePath, false). 
    end method.
    
    /**
    Add a buffer to the request - called for children of main table or when there is no main table 
    skip repos allows repos relation to be ignored for find  
    pClientRequest is only passed from main table
    
    RelativePath is empty if the buffer is a top buffer other wisse it may be the buffer serialize-name with parents relative path in front       
    **/
    method protected void AddTableRequestTree( prequests as ICollection, pClientRequest as IClientGetRequest, phBuffer as handle, pcRelativePath as character, plSkipRepos as logical):
        define variable oTableRequest    as GetDataTableRequest no-undo.
        define variable cFieldMap        as character no-undo. 
        define variable oFilterParser    as FilterParser no-undo.
        define variable cClientFilter    as character no-undo.
        define variable cClientSort      as character no-undo.
        oTableRequest = new GetDataTableRequest(phBuffer:table-handle:name).
        
        define variable oQueryDefinition as IQueryDefinition    no-undo.
        
        if valid-object(pClientRequest) and pClientRequest:Select > "" then
        do:
            oTableRequest:SelectFields = GetTTFieldsFromSelect(phBuffer, pClientRequest:Select, pcRelativePath).
        end.
        
        pRequests:Add(oTableRequest).
        if type-of(pClientRequest,IClientGetRequest) then 
        do:
            cClientFilter = cast(pClientRequest,IClientGetRequest):GetChildFilter(pcRelativePath).
            cClientSort  = cast(pClientRequest,IClientGetRequest):GetChildSort(pcRelativePath).
            if cClientFilter > ""  or cClientSort > "" then
            do: 
                oFilterParser    = new FilterParser(FieldMapHolder,pClientRequest:ExpandedRelations).
                oQueryDefinition = oFilterParser:GetQueryDefinition(phBuffer,cClientFilter,cClientSort).  
                
                if oFilterParser:HasWarnings then 
                do: 
                    Warnings = oFilterParser:Warnings. 
                end.    
/*                                                                                                                                  */
/*               // if cFilter > "" and pclientRequest:Wildcards then                                                               */
/*               // do:                                                                                                             */
/*                 //   new QueryWildcardConverter():Traverse(oQueryDefinition).                                                    */
/*               // end.                                                                                                            */
/*                // for a function we may have used keywhere to build a filter already so add it to the new definition             */
/*                if valid-object(oTableRequest:QueryDefinition) and valid-object(oTableRequest:QueryDefinition:QuerySelection) then*/
/*                    oQueryDefinition:AddEntry(oTableRequest:QueryDefinition:QuerySelection).                                      */
/*                                                                                                                                  */
                oTableRequest:QueryDefinition = oQueryDefinition.
            end.
        end.
        
        AddTableChildrenToRequestTree(prequests,pClientRequest, phBuffer, pcRelativePath, plSkipRepos). 
    end method.
    
    /**
    Add all child buffers to the request 
    pClientRequest is only passed from main table 
    **/
    method protected void AddTableChildrenToRequestTree( prequests as ICollection, pClientRequest as IClientGetRequest, phBuffer as handle,pcPath as character, plSkipRepos as logical ):
        define variable i         as integer no-undo.
        define variable hRelation as handle no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (i).
            if hRelation:recursive = false and (hRelation:reposition = false or plSkipRepos = false) then
            do:
                AddTableRequestTree(prequests,pClientRequest,hRelation:child-buffer,left-trim(pcPath + "/" + hRelation:child-buffer:serialize-name,"/"), plSkipRepos).
            end.
        end.         
    end method.
    
end class.
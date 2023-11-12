 
 /*------------------------------------------------------------------------
    File        : ValidationManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 30 12:54:33 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 
block-level on error undo, throw.

using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IFindService from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.ClientGetRequest from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
 

class Pmfo.Core.BusinessLogic.ValidationManager: 
    
    define public property Dataset     as handle    no-undo get. set.   
    define public property ParentPath  as character no-undo get. set.   
    define public property LookupPaths as character no-undo get. set.   
    define public property BindNames   as character no-undo get. set.   
    
     define protected property ServiceManager as IServiceManager no-undo 
         get():
            return cast(Application:ServiceManager,IServiceManager).
         end.
   
     define protected property FindService as IFindService no-undo 
         get():
            return cast(this-object:ServiceManager:getService(get-class(IFindService)),IFindService).
         end.
   
   
   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public ValidationManager (phDataset as handle ):
        super ().
        this-object:Dataset = phDataset.
    end constructor.
    
    constructor public ValidationManager (phDataset as handle, pcParentPath as character):
        this-object(phDataset).
        ParentPath = pcParentPath.    
    end constructor.
    
    constructor public ValidationManager (phDataset as handle, pcParentPath as character, pcLookupPaths as character):
        this-object(phDataset,pcParentPath).
        LookupPaths = pclookupPaths.
    end constructor.
    
    constructor public ValidationManager (phDataset as handle, pcParentPath as character, pcLookupPaths as character, pcBindNames as character):
        this-object(phDataset,pcParentPath,pcLookupPaths).
        BindNames = pcBindNames.
    end constructor.
    
    method public handle GetQuery(phBuffer as handle):
        return GetQuery(phbuffer,false).       
    end method.      
    
    method public handle GetQuery(phBuffer as handle, plShallow as logical):
        define variable oBuilder            as QueryRequestBuilder no-undo.
        define variable oRequest            as IGetDataRequest no-undo.
        define variable oClientRequest      as IClientGetRequest no-undo.
        define variable hQuery              as handle no-undo.      
        define variable hCallerBuffer       as handle    no-undo.
        define variable hBuffer             as handle    no-undo.
        define variable cQueryString        as character no-undo.
        define variable i                   as integer no-undo.
        define variable cPath               as character no-undo.
        define variable cCollections        as character no-undo.
        define variable cPathEntry          as character no-undo.
        define variable lWhereAdded         as logical no-undo.
        define variable cFullpath           as character no-undo.
        define variable hTempDataset        as handle no-undo.
        
        cPath         = BufferUtil:GetQualifiedBufferName(phBuffer).
        cFullpath = trim(subst("&1/&2",ParentPath,cPath),"/").
        oClientRequest = new ClientGetRequest("GET",cFullpath,new StringStringMap()). 
        oBuilder = new QueryRequestBuilder().
        oRequest = oBuilder:BuildGetDataRequest(oClientRequest, this-object:dataset).    
        
        create query hQuery.
        if ParentPath > "" then 
            hTempDataset = FetchParentDatasetAndAddParentsToQuery(phbuffer, hQuery,ParentPath).

        //  hCallerBuffer = DatasetUtil:GetBufferBySerializeName(pUpdateRequest:Dataset, pUpdateRequest:PublicName).
        cCollections   = Array:tostring(oClientRequest:Collections).
        
        // if Shallow only for the explicitly requested collections in ClientRequest      
        do i = 1 to num-entries(cFullPath,"/"):
            cPathEntry = entry(i,cPath,"/"). 
            lWhereAdded = false.    
            if lookup(cPathEntry,cCollections) = 0 then
            do:
                // if the requested path does not align there is nothing to do for this buffer
                if extent(oClientRequest:Collections) >= i then 
                    return ?.
                // or if shallow then there is nothing to do for this buffer
                else if plshallow then
                    return ?.
            end.
            
            if cPathentry = phbuffer:serialize-name then
                hBuffer = phbuffer.
            else
                hBuffer = DatasetUtil:GetBufferBySerializeName(this-object:dataset,cPathEntry).           
                 
            
            cQueryString = StringUtil:AppendEntry(cQueryString,subst("&1 each &2 ",
                                                                     if i = 1 then "for" else ",",
                                                                     hBuffer:name),
                                                                " ").
            
            if extent(oClientRequest:Collections[i]) >= i
            and oClientRequest:Collections[i] <> cPathEntry then
                // this probably means messed up logic above here...   
                undo, throw new IllegalStateError(subst("Qualifed path for '&1' does not match Request path entry '&2'. Please Contact IT.",cPathEntry,oClientRequest:Collections[i])).  
            
            if valid-handle(hBuffer:parent-relation) then
            do:
                cQueryString = StringUtil:AppendEntry(cQueryString,"where"," ").
                lWhereadded = true.
                
                cQueryString = StringUtil:AppendEntry(cQueryString,substring(hBuffer:parent-relation:where-string,7)," ").
            end.
            
            if extent(oClientRequest:Collections[i]) >= i then
            do:
                if extent(oClientRequest:KeyWheres) >= i and oClientRequest:KeyWheres[i] > "" then
                do:
                    cQueryString = StringUtil:AppendEntry(cQueryString,if lWhereAdded then "and" else "where"," ").
                    lWhereAdded = true.
                    cQueryString = StringUtil:AppendEntry(cQueryString,oRequest:TableRequests[i]:QueryDefinition:QuerySelection:Tostring()," "). 
                end.                       
            end.
            
            hQuery:add-buffer(hBuffer).
            if phBuffer:serialize-name = hBuffer:serialize-name then
                leave.
         end. 
         message "publciname" hBuffer:serialize-name skip "Querystring" cQueryString
         view-as alert-box.
         
        return ?.
        finally:
           if valid-object(hQuery) then 
               delete object hQuery.
           if valid-object(hTempDataset) then 
               delete object hTempDataset.
        end finally.    
                
    end method. 
    
    
    method protected handle FetchParentDatasetAndAddParentsToQuery(phbuffer as handle, phQuery as handle, pcParentPath as character ):
        define variable oBe                      as IBusinessEntity no-undo.
        define variable hDataset                 as handle no-undo.
        define variable cBottomUpGrandParentPath as character no-undo.
        define variable i                        as integer no-undo.
        define variable hQuery                   as handle  no-undo.
        define variable oLink                    as IRelation  no-undo.
        define variable cParentFields            as character no-undo.
        define variable cChildFields             as character no-undo.
        define variable cPairsList               as character no-undo.
        define variable cBreakBy                 as character no-undo.
        define variable hFields                  as handle extent no-undo.
        define variable oQueryGroup              as QueryGroup no-undo.
        define variable oTableRequests           as IGetDataTableRequest extent no-undo.
        define variable iGroupCount              as integer no-undo.
          
        oBe = this-object:ServiceManager:CreateBusinessEntity(entry(num-entries(pcParentPath,"/"),pcParentPath,"/")).
        
        do i = num-entries(pcParentPath,"/") - 1 to 2:
            // add the entry to the beginning so pass the list as last parameter 
            cBottomUpGrandParentPath = StringUtil:AppendEntry(entry(i,pcParentPath,"/"),cBottomUpGrandParentPath).
        end.
        oLink = oBe:GetLink(phbuffer:table-handle:serialize-name).
        cPairslist = oLink:PairsList().
        
        extent(hFields) = integer(num-entries(cPairsList) / 2).
        do i = 1 to num-entries(cPairsList) by 2:
            cParentFields = StringUtil:AppendEntry(cParentFields,entry(i,cPairsList)) .
            hfields[i]    = phbuffer:buffer-field(entry(i + 1,cPairsList)).
        end.    
        
        do i = 1 to num-entries(cChildFields):
            cBreakBy = StringUtil:AppendEntry(cBreakBy,entry(i,cChildFields)," by") . 
        end.
        
        hDataset = oBe:GetExpandDataset(cBottomUpGrandParentPath).
        extent(oTableRequests) = hDataset:num-buffers. 
        do i = 1 to hDataset:num-buffers:
            oTableRequests[i] = new GetDataTableRequest(hDataset:get-buffer-handle(i)).
            // use default buffer so that calls to static table will work  
            phQuery:add-buffer (hDataset:get-buffer-handle(i):table-handle:default-buffer-handle).
        end.
        create query hQuery.
        hQuery:add-buffer(phquery).
        hquery:query-prepare(subst("for each &1 break by &2",phBuffer:name,cBreakBy)).
        hQuery:query-open().
        hQuery:get-first.
        do while phBuffer:avail.
            if hQuery:last-of (extent(hFields)) then 
            do:
                oQueryGroup = new QueryGroup().
                do i = 1 to extent(hFields):
                    oQueryGroup:AddEntry(hFields[i]:name,QueryOperatorEnum:Eq,hFields[i]:buffer-value).
                end.
                iGroupCount = iGroupCount + 1.
                if iGroupCount > 1 then
                    oQueryGroup:Join = JoinEnum:Or.
                oTableRequests[1]:AddQueryEntry(oQueryGroup).
                
            end.    
            hQuery:get-next.
        end.
        
        oBe:GetData(new GetDataRequest(oTableRequests), output dataset-handle hDataset by-reference).     
         
        // keep until request is executed 
        return hDataset.
    end.     
    
    
   
end class.
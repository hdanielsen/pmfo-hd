 
 /*------------------------------------------------------------------------
    File        : RequestUtil
    Purpose     : create requests for dataset 
                  create dataset and get data for a top query 
                  create dataset and get data for a parent buffer 
                  
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Dec 22 10:39:01 EST 2023
    Notes       : 
                 TODO: consolidate overlap with ActionRequestUtil
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.DatasetUtil from propath.
 
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.Parser.BaseFilterParser from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Util.RequestUtilOptionEnum from propath. 
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationDefinition from propath.
using Pmfo.Core.BusinessLogic.Relation from propath.
using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.

class Pmfo.Util.RequestUtil: 
    // not sure of this should be a property... 
    define public static property IncludeObsoleteQuery as IQueryEntry
         get().
             if not valid-object(IncludeObsoleteQuery) then
                 IncludeObsoleteQuery = CreateIncludeObsoleteQuery("obsolete").
              
             return IncludeObsoleteQuery.
         end.    
         private set.
    
    define public static property IncludeDisabledQuery as IQueryEntry
         get().
             if not valid-object(IncludeDisabledQuery) then
                 IncludeDisabledQuery = CreateIncludeObsoleteQuery("disabled").
              
             return IncludeDisabledQuery.
         end.
         private set.       
    
    method private static IQueryEntry CreateIncludeObsoleteQuery(pcFieldname as character):
        define variable oQueryGroup as IQueryGroup no-undo.
        oQueryGroup = new QueryGroup().
        oQueryGroup:AddEntry(pcFieldname,QueryOperatorEnum:eq,true).
        oQueryGroup:AddEntry(JoinEnum:OR,pcFieldname,QueryOperatorEnum:eq,false).
        return oQueryGroup. 
    end method.     
         
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQuery as IQueryEntry): 
        return CreateRequestForDataset(phdataset,poTopQuery,0).
    end method. 
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQuery as IQueryEntry, piNumRecords as integer): 
        return CreateRequestForDataset(phdataset,poTopQuery,0,RequestUtilOptionEnum:None).
    end method.
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQuery as IQueryEntry, pOption as RequestUtilOptionEnum): 
        return CreateRequestForDataset(phdataset,poTopQuery,0,pOption).
    end method.
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQuery as IQueryEntry, piNumRecords as integer,pOption as RequestUtilOptionEnum): 
        define variable i as integer no-undo.
        define variable oTableRequests as IGetDataTableRequest extent  no-undo.
        define variable hBuffer as handle no-undo.
        define variable hobsolete as handle no-undo.
        extent(oTableRequests) = phDataset:num-buffers.
        do i = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(i).
            oTableRequests[i] = new GetDataTableRequest(hBuffer).
            if i = 1 then 
            do:
                if valid-object(poTopQuery) then
                    oTableRequests[i]:AddQueryEntry(poTopQuery).
                if piNumRecords > 0 then 
                    cast(oTableRequests[i],GetDataTableRequest):NumRecords = piNumRecords.
            end.  
            if RequestUtilOptionEnum:IncludeObsolete:Equals(pOption) then
            do:
                // TODO 
                // define public ObsoleteFieldName as character in BE.  
                //   add to entity.i similar to KeyList and add API in ServiceInfo the same way 
                //                  allow call from bufferUtil that can be used here for child buffers
                // fix the proteced in DataSource to call Serviceinfo
                   
                if hBuffer:table-handle:serialize-name = "psiusers" then
                    oTableRequests[i]:AddQueryEntry(IncludeDisabledQuery).
                else do: 
                    hObsolete = ?.
                    hObsolete = hBuffer:buffer-field("obsolete") no-error.
                    if valid-handle(hObsolete) then
                        oTableRequests[i]:AddQueryEntry(IncludeObsoleteQuery).
                end.    
            end.    
         //   else if RequestUtilOptionEnum:  
        end.              
        return new GetDataRequest(oTableRequests).
    end method. 
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQueryDefinition as IQueryDefinition): 
        return CreateRequestForDataset(phDataset,poTopQueryDefinition,0).
    end method. 
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle, poTopQueryDefinition as IQueryDefinition, piNumRecords as integer): 
        
        define variable i as integer no-undo.
        define variable oTableRequests as GetDataTableRequest extent  no-undo.
        Assert:NotNull(poTopQueryDefinition,"QueryDefinition").
        extent(oTableRequests) = phDataset:num-buffers.
        do i = 1 to phDataset:num-buffers:
            oTableRequests[i] = new GetDataTableRequest(phDataset:get-buffer-handle(i)).
            if i = 1 then 
            do:
                oTableRequests[i] = new GetDataTableRequest(phDataset:get-buffer-handle(i)).
                oTableRequests[1]:QueryDefinition = poTopQueryDefinition.
                if piNumRecords > 0 then
                oTableRequests[i]:NumRecords = pinumRecords.
            end.
        end.              
        return new GetDataRequest(oTableRequests).
    end method. 
    
    method public static IGetDataRequest CreateRequestForDataset(phDataset as handle):
        define variable oDummyQuery as IQueryEntry no-undo.
        return CreateRequestForDataset(phDataset,oDummyQuery).
    end method. 
    
    // the internal version will just ignore empty queries (which makes sense for Crerate*Forchild methods)
    // public callers will get an error (they can use CrateRequestDataset and then use that to call the BE)  
    method public static handle CreateDatasetAndGetData(pBe as IBusinessEntity,pcExpands as character,pTopQuery as IQueryEntry):
        return CreateDatasetAndGetData(pBe,pcExpands,pTopQuery,RequestUtilOptionEnum:None).
    end method. 
    
    method public static handle CreateDatasetAndGetData(pBe as IBusinessEntity,pcExpands as character,pTopQuery as IQueryEntry, input pRequestOption as RequestUtilOptionEnum):
        define variable oEntries as Ccs.BusinessLogic.IQueryEntry extent no-undo.
        Assert:NotNull(pTopQuery,"Query").
        if type-of(pTopQuery,Ccs.BusinessLogic.IQueryGroup) then
        do:
            oEntries = cast(pTopQuery,Ccs.BusinessLogic.IQueryGroup):Entries.
            Assert:IsTrue(extent(oEntries) <> ?,"QueryGroup has at least one predicate").
        end.    
        return CreateDatasetAndGetDataInternal(pBe,pcExpands,pTopQuery,pRequestOption).
    end method. 
    
        // the internal version will just ignore empty queries (which makes sense for Crerate*Forchild methods)
    // public callers will get an error (they can use CrateRequestDataset and then use that to call the BE)  
    method public static handle CreateDatasetAndGetData(pBe as IBusinessEntity,pcExpands as character,pcTopExpression as character):
        define variable oQueryDefinition as IQueryDefinition no-undo.
        define variable oFilterbuilder   as BaseFilterParser no-undo.
        define variable hDataset         as handle no-undo.
        define variable hBuffer          as handle no-undo.
        Assert:NotEmpty(pcTopExpression,"Expression").
        oFilterBuilder = new BaseFilterParser().
        
        hDataset = CreateDataset(pBe,pcExpands).
        hBuffer = hDataset:get-buffer-handle(1).
        
        oQueryDefinition = oFilterBuilder:GetQueryDefinition(hBuffer,pcTopExpression,"").
        
        pBe:GetData(CreateRequestForDataset(hDataset,oQueryDefinition),output dataset-handle hDataset by-reference).
        
        return hDataset.
    end method. 
    
    method private static handle CreateDataset(pBe as IBusinessEntity,pcExpands as character):
        define variable hDataset as handle no-undo.
        define variable hBuffer  as handle no-undo.
        if pcExpands > "" then
            hDataset = pBe:GetExpandDataset(pcExpands).
        else 
        do:
            if pBe:Datasets[1]:serialize-name <> pBe:Resource then 
               undo, throw new IllegalOperationError(subst("Cannot create dataset for Resource '&1'",pBe:Resource)).
            hBuffer = DatasetUtil:GetTopBuffer(pBe:Datasets[1], 1).
            if hBuffer:serialize-name <> pBe:Resource then 
                undo, throw new IllegalOperationError(subst("Cannot create dataset for Resource '&1'",pBe:Resource)).
      
            create dataset hDataset.
            hDataset:name = pBe:Resource.
            create buffer hBuffer for table hBuffer.
         
            hBuffer:serialize-name = pBe:Resource.
            hDataset:add-buffer (hBuffer).
 
        end.   
        return hDataset.
    end method.    
        
    method private static handle CreateDatasetAndGetDataInternal(pBe as IBusinessEntity,pcExpands as character,pTopQuery as IQueryEntry,input pRequestOption as RequestUtilOptionEnum):
        define variable hDataset as handle no-undo.
        define variable hbuffer  as handle no-undo. 
      
        Assert:NotNull(pTopQuery,"Query").
        hDataset =CreateDataset(pBe,pcExpands). 
        GetData(pBe,ptopQuery,hDataset,pRequestOption). 
 
        return hDataset.            
    end method.   
    
    method private static handle GetData(pBe as IBusinessEntity,pTopQuery as IQueryEntry,phDataset as handle, input pRequestOption as RequestUtilOptionEnum): 
         define variable oEntries as Ccs.BusinessLogic.IQueryEntry extent no-undo.
        
        Assert:NotNull(pTopQuery,"Query").
        Assert:NotNull(phDataset,"Dataset").
        Assert:IsTrue(phDataset:type = "dataset","Is a Dataset").
        
        // for sanity - avoid call below if empty  
        if type-of(pTopQuery,Ccs.BusinessLogic.IQueryGroup) then
            oEntries = cast(pTopQuery,Ccs.BusinessLogic.IQueryGroup):Entries.
        
        if (type-of(pTopQuery,Ccs.BusinessLogic.IQueryGroup) = false or extent(oEntries) <> ?) then 
            pBe:GetData(CreateRequestForDataset(phDataset,pTopQuery,pRequestOption),output dataset-handle phDataset by-reference).
        
    end method.    
    
    method public static handle CreateDatasetAndGetDataForChild(
        phParentHandle as handle,
        poRelationDef as IRelation,  
        pBe as IBusinessentity): 
            
        return CreateDatasetAndGetDataForChild(phParentHandle, poRelationDef, pBe,"",RequestUtilOptionEnum:None).
        
    end method.
    
     method public static handle CreateDatasetAndGetDataForChild(
        phParentHandle as handle,
        pcParentFields as character,
        pBe as IBusinessentity): 
        return CreateDatasetAndGetDataForChild(phParentHandle,pcParentFields, pBe,"").
    end method.
     
    method public static handle CreateDatasetAndGetDataForChild(
        phParentHandle as handle, 
        pcParentFields as character,
        pBe as IBusinessentity,
        pcExpandChild as character): 
         
        return CreateDatasetAndGetDataForChild(phParentHandle, pcParentFields, pBe,pcExpandChild,RequestUtilOptionEnum:None).
    end.
    
    method public static handle CreateDatasetAndGetDataForChild(phParentHandle as handle,
                                                                pcParentFields as character,
                                                                pBe as IBusinessentity,
                                                                pOption as RequestUtilOptionEnum): 
         return CreateDatasetAndGetDataForChild(phParentHandle, pcParentFields, pcParentFields,pBe,pOption).
    end method.
    
    method public static handle CreateDatasetAndGetDataForChild(phParentHandle as handle,
                                                                pcParentFields as character,
                                                                pcChildfields  as character,
                                                                pBe as IBusinessentity): 
    
        return CreateDatasetAndGetDataForChild(phParentHandle,
                                               pcParentFields,
                                               pcChildfields,   
                                               pBe,
                                               "", //expand
                                               RequestUtilOptionEnum:None). 
        
    end.
    
    method public static handle CreateDatasetAndGetDataForChild(phParentHandle as handle,
                                                                pcParentFields as character,
                                                                pcChildfields  as character,
                                                                pBe as IBusinessentity,
                                                                pOption as RequestUtilOptionEnum):                                                                                                    
    
        return CreateDatasetAndGetDataForChild(phParentHandle,
                                               pcParentFields,
                                               pcChildfields,   
                                               pBe,
                                               "", //expand
                                               pOption). 
        
    end.
    
    method public static handle CreateDatasetAndGetDataForChild(phParentHandle as handle,
                                                                pcParentFields as character,
                                                                pBe as IBusinessentity,
                                                                pcExpandChild as character,
                                                                pOption as RequestUtilOptionEnum): 
        return CreateDatasetAndGetDataForChild(phParentHandle, pcParentFields,pcParentFields, pBe,pcExpandChild,pOption).
    end method.
    
    method public static handle CreateDatasetAndGetDataForChild(phParentHandle as handle,
                                                                pcParentFields as character,
                                                                pcChildfields  as character,
                                                                pBe as IBusinessentity,
                                                                pcExpand as character,
                                                                pOption as RequestUtilOptionEnum): 
        
        define variable oRelation as IRelation no-undo.
        define variable i as integer no-undo.
        define variable cParentFields as character extent no-undo.
        define variable cChildFields as character extent no-undo.
        define variable cField as character no-undo.
        extent(cParentFields) = num-entries(pcParentFields).
        extent(cChildFields)  = extent(cParentFields).
        
        do i = 1 to extent(cParentFields):
            cfield = entry(i,pcParentFields).
            cParentFields[i] = entry(i,pcParentFields).
            cChildFields[i] = entry(i,pcChildFields).
        end.    
        
        oRelation = new Relation(phParentHandle:serialize-name,pBe:Resource,cParentFields,cChildFields).                                                            
        return CreateDatasetAndGetDataForChild(phParentHandle, oRelation, pBe,pcExpand,pOption).                                                           
    end method.
    
   
    
    // Fetch child data for all rows of a buffer and validate that all rows have a child  
    // TODO overloads to manage relation from parent to child
    method public static handle CreateDatasetAndGetDataForChild(
        phParentHandle as handle,
        poRelation as IRelation,
        pBe as IBusinessentity,
        pcExpandChild as character,
        pOption as RequestUtilOptionEnum):
            
        define variable oQuery        as IQueryGroup no-undo.
        define variable oRowQuery     as QueryGroup no-undo.
        define variable hQuery        as handle no-undo.
        define variable hFields       as handle extent no-undo.
        define variable iField        as integer no-undo.
        define variable iRow          as integer no-undo.
        define variable hDataset      as handle no-undo.
        define variable hChildbuffer  as handle no-undo.
        define variable lSkipRow      as logical no-undo. 
        define variable hParentBuffer as handle no-undo.
        define variable oJoins       as IJoinPredicate extent no-undo. 
        oJoins = poRelation:JoinPredicates. 
        
        case phParentHandle:type:
            when "buffer" then 
                hParentBuffer = phParentHandle. 
            when "temp-table" then 
                hParentBuffer = phParentHandle:default-buffer-handle. 
            otherwise 
                undo, throw new IllegalArgumentError(substitute("Unsupported handle type '&1'",phParentHandle:type)). 
        end case.
        
        extent(hFields) = extent(oJoins).
        do iField = 1 to extent(oJoins) 
        on error undo, throw:
            hFields[iField] = hParentBuffer:buffer-field(oJoins[iField]:ParentField).    
            catch e as Progress.Lang.Error :
                undo, throw new IllegalArgumentError(
                            subst("Cannot process &2 parameter with join using parent field &2",
                                   if valid-handle(hParentBuffer)
                                   then subst("buffer handle for &1",hParentBuffer:name)
                                   else "invalid",
                                   oJoins[iField]:ParentField
                                  ) 
                            ,
                            e).                        
            end catch.
        end.
        
        create query hQuery.
        hquery:add-buffer(hParentbuffer).
        hQuery:query-prepare(subst("for each &1",hParentBuffer:name)).
        hQuery:query-open.
        hQuery:get-first.
        oQuery = new QueryGroup().
         
        do while hParentBuffer:avail:
            lSkipRow = true.
            oRowQuery = new QueryGroup().
            do iField = 1 to extent(oJoins) :
                if (RequestUtilOptionEnum:SkipEmpty:Equals(pOption) 
                    and isEmpty(hFields[ifield])  
                    ) then
                do: 
                   // skip for any empty value  
                   lSkipRow = true.
                   leave.     
                end.
                // only skip if all values are null
                if hFields[iField]:buffer-value <> ? 
                then 
                do:
                    lSkipRow = false.
                end.
                oRowQuery:AddEntry(oJoins[iField]:ChildField,Ccs.BusinessLogic.QueryoperatorEnum:Eq,hFields[iField]:buffer-value ).
            end.
           
            if not lSkipRow then
            do: 
                if not oQuery:HasEntry(oRowQuery) then
                do:
                    irow = iRow + 1.
                    if iRow > 1 then
                       oRowQuery:Join = Ccs.BusinessLogic.JoinEnum:Or.
                       
                    oQuery:AddEntry(oRowQuery).
                end.   
            end.
            hQuery:get-next.
        end.        
        
        // INTERNAL will ignore query with no data - public will throw error 
        return CreateDatasetAndGetDataInternal(pBe,pcExpandChild,oQuery,pOption).
        
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.        
        end finally.
    end method.
    
    method private static logical isEmpty(phField as handle):
        if phField:buffer-value = ? then 
            return true.
        case phField:data-type:
            when "character" or when "longchar" then 
                return phField:buffer-value = "". 
            when "decimal" or when "integer" or when "int64" then 
                 return phField:buffer-value = 0.
        end case. 
        return false. 
    end.    
       
end class.
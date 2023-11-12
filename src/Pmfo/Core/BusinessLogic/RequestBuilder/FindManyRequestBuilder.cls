 
 /*------------------------------------------------------------------------
    File        : FindManyReqquestBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Oct 01 18:22:12 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.Common.INamedArrayHolder from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.Collections.Map from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Common.NamedArrayNameEnum from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using OpenEdge.Core.String from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.Collections.ICharacterHandleMap from propath.
using Pmfo.Core.Collections.CharacterHandleMap from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using OpenEdge.Core.WidgetHandle from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.RequestBuilder.FindManyRequestBuilder: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property ResourceDataset as handle no-undo get. set. 
    
    constructor public FindManyRequestBuilder (pResourceDataset as handle ):
        super ().
        ResourceDataset = pResourceDataset.
        
    end constructor.

    method public IGetDataRequest Build(poArrayValues as INamedArrayHolder, plGetOnlyRequestedTables as logical, phDataset as handle, output phInvolvedRelations as handle extent):
        define variable oTableFieldMap         as ICharacterHandleMap no-undo.
        define variable oBufferQueryMap        as IMap no-undo.
        define variable oBufferRequestMap      as IMap no-undo.
        define variable cNames                 as character extent no-undo.
        define variable cQualifier             as character no-undo.
        define variable cFieldName             as character no-undo.
        define variable oQuery                 as QueryGroup no-undo.
        define variable i                      as integer no-undo.
        define variable oRequest               as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oTableRequest          as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable hBuffer                as handle no-undo.
        define variable cDatasetMatchErrorTmpl as character init "Array Name '&1' does not match &2 name '&3'" no-undo.
        define variable cBufferMatchErrorTmpl  as character init "Array name '&1' is not a valid &2 name in the output dataset" no-undo.
        define variable IField                 as integer no-undo.
        define variable ILength                as integer no-undo.
        define variable hFields                as handle extent no-undo.
        define variable cField                 as character no-undo.
        define variable cChildField            as character no-undo.
        define variable cParentField           as character no-undo.
        define variable lFirstRequest          as logical no-undo.
        define variable iBuffer                as integer no-undo.
        define variable cNamesForBuffer        as character extent no-undo.
        define variable oArrayHolder           as IPrimitiveArrayHolder no-undo.
        define variable iExtent                as integer no-undo. 
        define variable hField                 as handle no-undo.
         
        if not valid-handle(phDataset) then  
            undo, throw new IllegalArgumentError("Output dataset is not valid").
        
        cNames = poArrayValues:GetNames().
       
        extent(hFields) = extent(cNames).
        
        oTableFieldMap = new CharacterHandleMap().
        oRequest = new GetDataRequest().
        oBufferQueryMap   = new Map(). 
        oBufferRequestMap = new Map(). 
        do i = 1 to extent(cNames):
            if num-entries(cNames[i],".") <> 2 then 
                undo, throw new UnsupportedOperationError(subst("GetData for NameArrayHolder with Name '&1'. The name must be a qualified field name with period.",
                                                                cNames[i]
                                                                )
                                                          ).
        
            cQualifier = entry(1,cNames[i],".").
            
            case poArrayValues:NameQualifier:
                when NamedArrayNameEnum:Public then
                do:
                    hBuffer = DatasetUtil:GetBufferByTableSerializeName(phDataset, cQualifier).
                    if not valid-handle(hBuffer) then 
                        undo, throw new IllegalArgumentError(subst(cBufferMatchErrorTmpl,
                                                                   cQualifier,
                                                                   lc(poArrayValues:NameQualifier:ToString())
                                                                   )
                                                             ).
                    if i = 1 then 
                    do:
                        if cQualifier <> ResourceDataset:get-buffer-handle(1):table-handle:serialize-name then 
                            undo, throw new IllegalArgumentError(subst(cDatasetMatchErrorTmpl,
                                                                       cQualifier,
                                                                       lc(poArrayValues:NameQualifier:ToString()),
                                                                       ResourceDataset:get-buffer-handle(1):table-handle:serialize-name
                                                                       )
                                                                  ).
                    end.    
                                                             
                end.
                when NamedArrayNameEnum:Table then
                do:
                    hBuffer = DatasetUtil:GetBufferByTableName(phDataset, cQualifier).
                    if not valid-handle(hBuffer) then 
                        undo, throw new IllegalArgumentError(subst(cBufferMatchErrorTmpl,
                                                                   cQualifier,
                                                                   lc(poArrayValues:NameQualifier:ToString())
                                                                   )
                                                             ).
                   
                    if i = 1 then 
                    do:
                        if cQualifier <> ResourceDataset:get-buffer-handle(1):table-handle:name then 
                            undo, throw new IllegalArgumentError(subst(cDatasetMatchErrorTmpl,
                                                                       cQualifier,
                                                                       lc(poArrayValues:NameQualifier:ToString()),
                                                                       ResourceDataset:get-buffer-handle(1):table-handle:name
                                                                       )
                                                                  ).
                    end.                                         
                end.           
                when NamedArrayNameEnum:Buffer then
                do:
                    hBuffer = phDataset:get-buffer-handle(cQualifier).
                    if not valid-handle(hBuffer) then 
                        undo, throw new IllegalArgumentError(subst(cBufferMatchErrorTmpl,
                                                                   cQualifier,
                                                                   lc(poArrayValues:NameQualifier:ToString())
                                                                   )
                                                             ).
                    if i = 1 then 
                    do:
                        if cQualifier <> ResourceDataset:get-buffer-handle(1):name then 
                            undo, throw new IllegalArgumentError(subst(cDatasetMatchErrorTmpl,
                                                                       cQualifier,
                                                                       lc(poArrayValues:NameQualifier:ToString()),
                                                                       ResourceDataset:get-buffer-handle(1):name
                                                                       )
                                                                  ).
                    end.                                            
                end.           
                when NamedArrayNameEnum:BufferSerializeName then
                do:
                    hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset,cQualifier).  
                    if not valid-handle(hBuffer) then 
                        undo, throw new IllegalArgumentError(subst(cBufferMatchErrorTmpl,
                                                                   cQualifier,
                                                                   "buffer serialize-name"
                                                                   )
                                                             ).
           
                end.
                otherwise 
                    undo, throw new UnsupportedOperationError(subst("Named Array Holder with Name Qualifier '&1'",
                                                                    poArrayValues:NameQualifier:ToString() 
                                                                    )
                                                              ).
                       
            end case.
            cField = entry(2,cNames[i],".").
            if NamedArrayNameEnum:Public:Equals(poArrayValues:NameQualifier) 
            or NamedArrayNameEnum:BufferSerializeName:Equals(poArrayValues:NameQualifier) 
            then 
            do:
                // TODO: cache field map (see filter and otherrequests)
                do iField = 1 to hBuffer:num-fields:
                    if hBuffer:buffer-field(i):serialize-name = cField then
                    do:
                        hFields[i] = hBuffer:buffer-field(i).
                        leave. 
                    end.    
                end.
                if not valid-handle(hFields[i]) then    
                    undo, throw new IllegalArgumentError(subst("Field Name '&1' is not a valid for entity '&2'",
                                                               cField,
                                                               cQualifier)).     
            end.
            else do:
                hFields[i] = hBuffer:buffer-field(cField) no-error.
              
                if not valid-handle(hFields[i]) then
                    undo, throw new IllegalArgumentError(subst("Field Name '&1' is not a valid for &2 '&3'",
                                                               cField,
                                                               poArrayValues:NameQualifier:ToString(),
                                                               cQualifier)).    
            end. 
            if i = 1 then
                iLength = PrimitiveHolderUtil:GetNumExtents(poArrayValues:GetHolder(string(cNames[i]))).
            else
                if iLength <> PrimitiveHolderUtil:GetNumExtents(poArrayValues:GetHolder(string(cNames[i]))) then
                    undo, throw new IllegalStateError(subst("The Named Arrays for '&1' has &2 extents while The Arrya for '&3' has &4. All the arrays must have the same number of extents",
                                                            cNames[i], 
                                                            PrimitiveHolderUtil:GetNumExtents(poArrayValues:GetHolder(string(cNames[i]))),
                                                            cNames[1], 
                                                            iLength
                                                            )
                                                      ).
                
            // store the field handles with buffer name field name as key to use when 
            // looking up parent value for buffer query
            
            oTableFieldMap:put(cNames[i],hFields[i]).
            
            // one request per table `
            oTableRequest = cast(oRequest:GetTableRequest(hBuffer),IGetDataTableRequest).
            if not valid-object(oTableRequest) then 
            do: 
                oTableRequest = new GetDataTableRequest(hBuffer).
                oRequest:AddTableRequest(oTableRequest).
            end. 
        end.
       
        // use a second loop so that we have a complete TableFieldMap 
        do iBuffer = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(iBuffer).
            
            oTableRequest = cast(oRequest:GetTableRequest(hBuffer),IGetDataTableRequest).
            if valid-object(oTableRequest) then 
            do: 
                do iExtent = 1 to iLength:
                    oQuery = new QueryGroup().
                    if valid-handle(hBuffer:parent-relation) then 
                    do:
                        do iField = 1 to num-entries(hBuffer:parent-relation:relation-fields) by 2: 
                            cParentField = entry(iField,hBuffer:parent-relation:relation-fields).
                            cChildField  = entry(iField + 1,hBuffer:parent-relation:relation-fields).
                            oArrayHolder = poArrayValues:GetHolderForField(hBuffer:parent-relation:parent-buffer:buffer-field(cParentField)).
                            if not valid-object(oArrayholder) then
                                undo, throw new IllegalStateError(subst("Missing Named Array for parent field &1.&2 needed to join to &3.&4",
                                                                        hBuffer:name,
                                                                        cChildField, 
                                                                        hBuffer:parent-relation:parent-buffer:name,
                                                                        cParentField
                                                                        )
                                                                  ).
                               
                            oQuery:AddEntry(cChildField, QueryOperatorEnum:Eq,PrimitiveHolderUtil:GetExtentValue(oArrayHolder, iExtent)).
                        end. 
                        extent(phInvolvedRelations) = (if extent(phInvolvedRelations) = ? then 0 else extent(phInvolvedRelations)) + 1.
                        phInvolvedRelations[extent(phInvolvedRelations)] = hBuffer:parent-relation.   
                    end.
            
                    cNamesForBuffer = poArrayValues:GetNamesForBuffer(hBuffer).
                    
                    do i = 1 to extent(cNamesForBuffer).
                        oArrayHolder = poArrayValues:GetHolder(string(cNamesForBuffer[i])).     
                        hField = oTableFieldMap:Get(cNamesForBuffer[i]).   
                        if not valid-handle(hField) then
                            undo, throw new IllegalStateError(subst("Field handle for &1 not in Map",cNamesForBuffer[i])).
                        oQuery:AddEntry(hField:name, QueryOperatorEnum:Eq,PrimitiveHolderUtil:GetExtentValue(oArrayHolder, iExtent)).
                    end.                    
                  
                    if iExtent > 1 then 
                        oQuery:Join = JoinEnum:Or.
                    oTableRequest:AddQueryEntry(oQuery).
                end.
            end.
            else if plGetOnlyRequestedTables = false then   
            do: 
                oTableRequest = new GetDataTableRequest(hBuffer).
                oRequest:AddTableRequest(oTableRequest).
            end.    
        end.
        return oRequest.
        
    end method.      
        
    
    
end class.
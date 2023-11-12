 
 /*------------------------------------------------------------------------
    File        : TableUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Oct 28 21:54:18 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Util.TableUtil: 
    
    method public static handle CreateTableLike(phBuffer as handle, pcTablename as character):
        define variable hDummyfields as handle extent no-undo.
        define variable cDummy as character no-undo.
        return CreateTableLike(phBuffer,pcTableName,hDummyFields,false, output cDummy).    
    end method.     
    
    method public static handle CreateTableLike(phBuffer as handle, pcTablename as character, pCopyfields as handle extent, plAddindex as logical, output pcfieldPairs as character):
        define variable hTable         as handle no-undo.
        define variable hBeforeTable   as handle no-undo.
        define variable iField         as integer no-undo.
        define variable cIndexname     as character no-undo.
        define variable cindexnames    as character no-undo.
        define variable lAnyAdded      as logical no-undo.
        define variable iIdxnamesuffix as integer no-undo.
        define variable hFld           as handle no-undo.
        define variable cFldName       as character no-undo.
        define variable iTry as integer no-undo.
        if not valid-handle(phBuffer) then
            undo, throw  new IllegalArgumentError("Invalid buffer").
    //    DeepCopy(phBuffer, output table-handle hTable) - need to empty also.
   
        define variable hBuffer as handle no-undo.
        create temp-table hTable.
        hTable:create-like(phBuffer).
        do iField = 1 to extent(pCopyFields):
            // rename new field is already defined 
            // i.e. error is expected
            cFLdName = pCopyFields[iField]:name.
            do while true:  
                iTry = iTry + 1. // will only use when ge 2
                hFld =  phBuffer:buffer-field(cFLdName) no-error.
                if not valid-handle(hFld) then 
                   leave.
                else 
                   cFldname = subst("&1-&2",pCopyFields[iField]:name,iTry).
            end.
            hTable:add-new-field(cFldName, pCopyFields[iField]:data-type).
            pcFieldPairs = pcFieldPairs 
                         + (if iField = 1 then "" else ",")
                         + pCopyFields[iField]:name 
                         + ","
                         + cFldname.
        end.
        if lanyAdded and plAddindex and extent(pCopyFields) > 0 then 
        do: 
            cindexnames = BufferUtil:GetIndexNames(phBuffer).
            do while true:
                iidxnamesuffix = iidxnamesuffix +  1.
                cindexname = subst("&1-idx&2",pCopyFields[1]:name,if iIdxnamesuffix = 1 then "" else string(iIdxnamesuffix)).
                if lookup(cindexname,cindexnames) = 0 then
                    leave.
            end.    
            hTable:add-new-index(cindexname,no,no). // not unique not primary (probably unnecessary as false is likely default, but the doc is not claar)
            do iField = 1 to extent(pCopyFields):
               hTable:add-index-field(cindexname,pCopyFields[iField]:name).
            end.     
        end.
        hTable:temp-table-prepare(pcTablename,valid-handle(phBuffer:table-handle:before-table)).
        hTable:serialize-name = phBuffer:table-handle:serialize-name.
        do iField = 1 to extent(pCopyFields):
            hTable:default-buffer-handle:buffer-field(pCopyFields[iField]:name):serialize-name = pCopyFields[iField]:serialize-name.
        end.
        return hTable.    
    end method.     
    
    method public static handle CreateTableLike(phBuffer as handle):
        return CreateTableLike(phBuffer,phBuffer:table-handle:name).
    end method.     
  
    method public static int64 CopyTable(phToHandle as handle,phFromHandle as handle):
        return CopyTable(phToHandle, phFromHandle, "").
    end method.    
    
    method public static int64 CopyTable(phToHandle as handle,phFromHandle as handle, pcCopyMode as character):
        define variable hQuery      as handle no-undo.
        define variable hToBuffer   as handle no-undo.
        define variable hFromBuffer as handle no-undo.
        define variable iCount      as int64 no-undo.
        define variable cKeys       as character no-undo.
        define variable hFromFlds   as handle extent no-undo.
        define variable iFld        as integer no-undo.
        define variable cFld        as character no-undo.
        define variable cFindTmpl   as character no-undo.
        define variable cWhereOrAnd as character no-undo.
        define variable cValues     as character extent no-undo.
        define variable lCreate     as logical no-undo.
        define variable lCopy       as logical no-undo.
        
        if lookup(pcCopyMode,",replace,merge") = 0 then
            undo, throw new IllegalArgumentError("Copy Mode must be 'replace' or 'merge' or blank").
        
        
        if phFromHandle:type <> phToHandle:type then 
            undo, throw new UnsupportedOperationError("Copy From and To handles of different type").
            
        case phToHandle:type:
            when "buffer" then 
            do:
                hToBuffer = phToHandle.
                hFromBuffer = phFromHandle.
            end. // buffer
            when "temp-table" then
            do:
                hToBuffer = phToHandle:default-buffer-handle.
                hFromBuffer = phFromHandle:default-buffer-handle.
            
            end.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Copy From and To is of type &1",phToHandle:type)).
        end.         
        if hToBuffer:table-handle = hFromBuffer:table-handle then
            undo, throw new IllegalArgumentError("Copy From and To handles are the same table").
        
        if pcCopyMode = "" and hToBuffer:table-handle:has-records then 
            undo, throw new UnsupportedOperationError(subst("The &1 table has data. Use CopyMode 'replace' or 'merge' to copy to a table with data",hToBuffer:name)).
            
        if pcCopyMode > "" then 
        do:
            cKeys = ServiceInfo:GetKeys(hToBuffer).
            extent(hFromFlds) = num-entries(cKeys).
            extent(cValues)  = extent(hFromFlds).
            cWhereOrAnd = "where".
            do iFld = 1 to extent(hFromFLds):
                cFld = entry(iFld,cKeys).
                hFromflds[iFld] = hFromBuffer:buffer-field(cFLd).
                cFindtmpl = cFindTmpl + subst(" &1 &2.&3 eq &4",cWhereOrAnd,hToBuffer:name,hFromFlds[iFld]:name,"&" + string(iFld)).
                cWhereOrAnd = "and".
            end.     
            cFindTmpl =  left-trim(cFindTmpl).    
        end.     
        
        create query hQuery. 
        hQuery:add-buffer(hFromBuffer).
        hQuery:query-prepare(subst("for each &1",hFromBuffer:name)).
        hQuery:query-open.
        hQuery:get-first.
        
        do while hFromBuffer:avail:
                
            if pcCopyMode = "" then
            do:
                lCreate = true.
                lCopy = true.
            end.
            else do:    
                do iFld = 1 to extent(hFromflds):
                    cValues[iFld] = if hFromFlds[iFld]:data-type = "character" then quoter(hFromFlds[ifld]:buffer-value) else hFromFlds[ifld]:buffer-value.
                    if cValues[iFLd] = ? then cValues[iFld] = "?". // substitute below  will ignore if ? 
                end.  
                hTOBuffer:buffer-release(). 
                do on error undo, throw:
                    hTOBuffer:find-unique(Array:SubstituteArray(cFindTmpl, cValues)).
                    catch e as Progress.Lang.Error :
                        // 138 is expected 
                        if e:GetMessageNum(1) <> 138 then 
                           undo, throw e.    
                    end catch.
                end.
                if hToBuffer:avail then 
                do: 
                    lCreate = false.
                    lCopy = pcCopyMode = "replace". 
                end.
                else 
                do:
                    lCreate = true.
                    lCopy = true.    
                end.   
            end.
  
            if lCopy then
            do:
                if lCreate then 
                do:
                    hToBuffer:buffer-create().
                end.
                hToBuffer:buffer-copy(hFromBuffer).
                iCount = iCount + 1.
            end.
            hQuery:get-next. 
        end. 
        return iCount.    
        finally:
            if valid-handle(hQuery) then 
                delete object hQuery.
        end.     
    end method.    
    
    method public static int64 CopyTableFromDataset(phToHandle as handle,phFromDataset as handle):
        define variable hQuery as handle no-undo.
        define variable hToBuffer as handle no-undo.
        define variable hFromBuffer as handle no-undo.
        define variable iCount as int64 no-undo.
        
        if phFromDataset:type <> "dataset" then 
            undo, throw new UnsupportedOperationError("From Dataset is not a dataset").
            
        case phToHandle:type:
            when "buffer" then 
            do:
                hToBuffer = phToHandle.
            end. // buffer
            when "temp-table" then
            do:
                hToBuffer = phToHandle:default-buffer-handle.
            end.
            otherwise 
               undo, throw new IllegalArgumentError(subst("Copy To is of type &1",phToHandle:type)).
        end.        
        hFromBuffer = DatasetUtil:GetBufferByTableName(phFromDataset, hToBuffer:table-handle:name). 
        return CopyTable(hFromBuffer,hToBuffer).
              
    end method.    
    
     
    
end class.
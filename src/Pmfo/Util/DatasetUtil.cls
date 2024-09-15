 
 /*------------------------------------------------------------------------
    File        : DatasetUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 02 09:21:58 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using Pmfo.Util.TableUtil from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.

class Pmfo.Util.DatasetUtil:
     
    method public static handle GetBufferBySerializeName(pDataset as handle, pserializename as char):
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        do i = 1 to pDataset:num-buffers:
            hbuffer = pDataset:get-buffer-handle(i).
            if hbuffer:serialize-name = pserializename then 
                return hBuffer. 
        end.    
        return ?.
    end method.
    
    method public static handle GetBufferByTableSerializeName(pDataset as handle, pserializename as char):
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        do i = 1 to pDataset:num-buffers:
            hbuffer = pDataset:get-buffer-handle(i).
            if hbuffer:table-handle:serialize-name = pserializename then 
                return hBuffer. 
        end.    
        return ?.
    end method.
    
    method public static handle GetBufferByPathName(pDataset as handle, pcQualName as character):
        
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable cRelativeQualifier as character no-undo.
        do i = 1 to pDataset:num-top-buffers:
            hbuffer = pDataset:get-top-buffer(i).
            if index(pcQualName,"/") = 0 
            and hbuffer:serialize-name = pcQualName then 
                return hBuffer. 
            else do:
                if hbuffer:serialize-name = entry(1,pcQualName,"/") then 
                do: 
                    cRelativeQualifier = pcQualName.
                    entry(1,cRelativeQualifier,"/") = "".
                    cRelativeQualifier = left-trim(cRelativeQualifier,"/").
                    return BufferUtil:GetBufferFromUrlQualifier(hBuffer, cRelativeQualifier).
                end.
            end.        
        end.    
        return ?.
    end method.
    
    method public static handle GetBufferByTableName(pDataset as handle, pTableName as char):
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        do i = 1 to pDataset:num-buffers:
            hbuffer = pDataset:get-buffer-handle(i).
            if hbuffer:table-handle:name = pTableName then 
                return hBuffer. 
        end.    
        return ?.
    end method.
    
    method public static handle extent GetBuffersByTableName(pDataset as handle, pTableNames as char):
        return GetBuffersByTableName(pDataset, pTableNames, false).
    end method.
    
    method public static handle extent GetBuffersByTableName(pDataset as handle, pTableNames as char, plDatasetOrder as logical):
        define variable i as integer no-undo.
  
        define variable hBuffers as handle extent no-undo.
        define variable hBuffer as handle no-undo.
        define variable iNum as integer no-undo.
        define variable iLookup as integer no-undo.
        
        extent(hBuffers) = num-entries(pTableNames).
        do i = 1 to pDataset:num-buffers:
            hBuffer = pDataset:get-buffer-handle(i).
            iLookup = lookup(pDataset:get-buffer-handle(i):name,pTablenames).
            if iLookup > 0 then 
            do:
                if plDatasetOrder = true then 
                    inum = iNum + 1.
                else 
                    inum = iLookup.   
               
                hBuffers[iNum] = hBuffer.
            end.     
        end.    
        return hBuffers.
    end method.
    
    method public static handle GetTopBufferBySerializeName(pDataset as handle, pserializename as char):
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable cFakeTopHandles as character no-undo.
        do i = 1 to pDataset:num-relations:
            // these are included in num-top-buffer - don't count them ehere
            if pDataset:get-relation (i):active = false 
            or pDataset:get-relation (i):reposition = true then
            do:
                cFaketopHandles = cFakeTopHandles 
                                + (if cFakeTopHandles = "" then "" else ",")
                                + string(pDataset:get-relation(i):child-buffer).
            end.                 
        end.
        
        do i = 1 to pDataset:num-top-buffers:
            hbuffer = pDataset:get-top-buffer(i).
            if hbuffer:serialize-name = pserializename  
            and lookup(string(hBuffer),cFakeTopHandles) = 0 then
                return hBuffer. 
        end.    
        return ?.
    end method.
    
    method public static handle GetTopBufferByTableName(pDataset as handle, pTableName as char):
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable cFakeTopHandles as character no-undo.
        do i = 1 to pDataset:num-relations:
            // these are included in num-top-buffer - don't count them ehere
            if pDataset:get-relation (i):active = false 
            or pDataset:get-relation (i):reposition = true then
            do:
                cFaketopHandles = cFakeTopHandles 
                                + (if cFakeTopHandles = "" then "" else ",")
                                + string(pDataset:get-relation(i):child-buffer).
            end.                 
        end.
        
        do i = 1 to pDataset:num-top-buffers:
            hbuffer = pDataset:get-top-buffer(i).
            if hbuffer:table-handle:name = pTableName 
            and lookup(string(hBuffer),cFakeTopHandles) = 0 then  
                return hBuffer. 
        end.    
        return ?.
    end method.
    
    // true num top-buffers - exclude repos and buffers that are in top-buffers because they are deactivated  
    
    method public static integer GetNumTopBuffers(pDataset as handle):
        define variable i            as integer no-undo.
        define variable iSkipcount   as integer no-undo.
        do i = 1 to pDataset:num-relations:
            if pDataset:get-relation (i):active = false 
            or pDataset:get-relation (i):reposition = true then
            do:
                iSkipCount = iSkipCount + 1.       
            end.                 
        end.
        return pDataset:num-top-buffers - iSkipCount.
        
    end method.
    
    // true num top-buffers - exclude repos and chiuld buffer on top because they are deactivated  
    method public static character GetDeactiveTopBuffers(pDataset as handle):
        define variable i            as integer no-undo.
        define variable cSkip        as character no-undo.
        define variable cDlm         as character no-undo.
        
        do i = 1 to pDataset:num-relations:
            if pDataset:get-relation (i):active = false 
            or pDataset:get-relation (i):reposition = true then
            do:
                assign
                    cSkip = cSkip
                          + cDlm
                          + string(pDataset:get-relation (i):child-buffer)
                    cDlm = ","
                    .             
            end.                 
        end.
        return cSkip.
    end method.
    
    // true top-buffer - exclude repos and in top-buffers because they are deactivated  
    method public static handle GetTopBuffer(pDataset as handle,pNum as int):
        define variable cSkip    as character no-undo.
        define variable iCount   as integer no-undo.
        define variable i        as integer no-undo.
        define variable hBuffer  as handle no-undo.
        cSkip = GetDeactiveTopBuffers(pDataset).
        if pnum < 1 then
            undo, throw new IllegalArgumentError("Number must be creater than 0").
        
        do i = 1 to pDataset:num-top-buffers:
            hbuffer = pDataset:get-top-buffer(i).
            // this may happen if called during completion i.e. BE:ToString
            // for now just return the bad handle and let the caller deal with it??   
            if hBuffer = ? or lookup(string(hBuffer),cSkip) = 0 then
            do:
                iCount = iCount + 1.
                
                if iCount = pnum then
                   return hBuffer.   
            end.
        end.
        undo, throw new IllegalArgumentError(subst("&1 is out of range of active top buffers",pnum)).
                 
    end method.
    
   
    
    
    // get a specific top buffer by top buffer index  not counting repos relations
    //  NOTEL this will return deactivated child bufffers and is typixally only needed
    // when the relation is purposedly actived/deactivated 
    // - for example allow update with multiple but allowing activation  relation before save 
    //   when a single record is posted by the client to be duplicated as child of many on save    
    method public static handle GetTopBufferExcludeRepos(pDataset as handle,piNum as integer):
        define variable i        as integer   no-undo.
        define variable hBuffer  as handle    no-undo.
        define variable iCount   as integer   no-undo.
        define variable cFakeTopHandles as character no-undo.
        do i = 1 to pDataset:num-relations:
            // these are included in num-top-buffer - don't count them ehere
            if pDataset:get-relation (i):reposition = true then
            do:
                cFaketopHandles = cFakeTopHandles 
                                + (if cFakeTopHandles = "" then "" else ",")
                                + string(pDataset:get-relation(i):child-buffer).
            end.                 
        end.
        
        do i = 1 to pDataset:num-top-buffers:
            hbuffer = pDataset:get-top-buffer(i).
            if  valid-handle(hBuffer:parent-relation)  = false
            and lookup(string(hBuffer),cFakeTopHandles) = 0 then
            do:
                iCount = iCount + 1.
                if iCount = pinum then
                    return hbuffer.
            end.  
        end.    
        return ?.
    end method.
    
    // count top buffers , but not repos ir deactivated relations 
    //  NOTEL this will return deactivated child bufffers and is typixally only needed
    // when the relation is purposedly actived/deactivated 
    // - for example allow update with multiple but allowing activation  relation before save 
    //   when a single record is posted by the client to be duplicated as child of many on save    
    method public static integer GetNumTopBuffersExcludeRepos(pDataset as handle):
        define variable i            as integer no-undo.
        define variable iSkipcount   as integer no-undo.
        
        // these are included in num-top-buffer 
        do i = 1 to pDataset:num-relations:
            if pDataset:get-relation (i):reposition = true then
            do:
                iSkipCount = iSkipCount + 1.       
            end.                 
        end.
        
        return pDataset:num-top-buffers - iSkipCount.
        
    end method.
    
    // return a comm separated list of deactivated child buffers 
    method public static character GetDeactivatedSerializeNames(pDataset as handle):
        define variable i          as integer   no-undo.
        define variable hRelation  as handle    no-undo.
        define variable cList as character no-undo. 
        do i = 1 to pDataset:num-relations:
            hRelation = pDataset:get-relation (i).
            if hRelation:active = false then
            do:
                cList = cList 
                      + (if cList = "" then "" else ",")
                      + hRelation:child-buffer:serialize-name.
            end.  
        end.    
        return cList.
    end method.
    
   
    method public static handle GetRelation(pDataset as handle, pcParent as char, pcChild as char):
        define variable i          as integer   no-undo.
        define variable hRelation  as handle    no-undo.
        do i = 1 to pDataset:num-relations:
            hRelation = pDataset:get-relation (i).
            if hRelation:parent-buffer:serialize-name = pcPArent 
            and hRelation:child-buffer:serialize-name  = pcChild then 
                return hRelation.
        end.  
        
    end method.
    
    
    method public static logical SetTrackingChanges(pDataset as handle,plTrack as logical):
        define variable i          as integer   no-undo.
        define variable hBuffer    as handle    no-undo.
        define variable lAny       as logical no-undo.
        do i = 1 to pDataset:num-buffers:
            hbuffer = pDataset:get-buffer-handle(i).
            if valid-handle(hBuffer:before-buffer) then
            do: 
                hBuffer:table-handle:tracking-changes = plTrack.
                lAny = true.
            end.
        end.    
        return lAny.
    end.
/*                                                                                                        */
/*    method public static handle CreateDataset (phOriginalBuffer as handle):                             */
/*        define variable hDataset as handle no-undo.                                                     */
/*        create dataset hDataset.                                                                        */
/*        define variable hBuffer as handle no-undo.                                                      */
/*        create buffer hBuffer for table phOriginalBuffer buffer-name phOriginalBuffer:table-handle:name.*/
/*                                                                                                        */
/*        hDataset:add-buffer(hBuffer).                                                                   */
/*        hBuffer:serialize-name =  phOriginalBuffer:serialize-name.                                      */
/*        CopyChildRelations(hDataset, hBuffer, phOriginalBuffer).                                        */
/*        return hDataset.                                                                                */
/*    end method.                                                                                         */
/*                                                                                                        */
        
    method public static void CopyParentRelation (pNewdataset as handle,phNewChild as handle, phOriginalChild as handle):
        define variable hOriginalBuffer as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable hRelation       as handle no-undo.
        
        hRelation = phOriginalChild:parent-relation.
        if valid-handle(hRelation) then
        do:
            hOriginalBuffer = hRelation:parent-buffer.
            create buffer hBuffer for table hOriginalBuffer buffer-name hOriginalBuffer:table-handle:name.
            hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
            pNewdataset:add-buffer(hBuffer).
            pNewDataset:add-relation(hBuffer,phNewChild,hRelation:relation-fields,hRelation:reposition,true).
            CopyParentRelation(pNewDataset,hBuffer,hOriginalBuffer). 
        end.
        catch e as Progress.Lang.Error :
            if valid-handle(hBuffer) then 
                delete object hBuffer.     
            undo, throw e.     
        end catch.
    end method. 
    
    method public static character GetParentTableNames (phBuffer as handle):
        define variable cNames as character no-undo.
        define variable cChildNames as character no-undo.
        define variable hRelation as handle no-undo.
        
        hRelation = phBuffer:parent-relation.
        if valid-handle(hRelation) then
        do:
            cNames = StringUtil:AppendEntry(hRelation:parent-buffer:table-handle:name, GetParentTableNames(hRelation:parent-buffer)). 
        end.
        return cNames. 
    end method. 
    
    method public static void CopyChildRelations (pNewdataset as handle,phNewParent as handle, phOriginalParent as handle):
        define variable i as integer no-undo.
        define variable hOriginalBuffer as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable hRelation       as handle no-undo.
        do i = 1 to phOriginalParent:num-child-relations:
            hRelation       = phOriginalParent:get-child-relation (i).
            hOriginalBuffer = hRelation:child-buffer.
            create buffer hBuffer for table hOriginalBuffer buffer-name hOriginalBuffer:table-handle:name.
            hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
            pNewdataset:add-buffer(hBuffer).
            pNewDataset:add-relation(phNewParent,hBuffer,hRelation:relation-fields,hRelation:reposition,true).
            CopyChildRelations(pNewDataset,hBuffer,hOriginalBuffer). 
        end.
        catch e as Progress.Lang.Error :
            if valid-handle(hBuffer) then 
                delete object hBuffer.     
            undo, throw e.     
        end catch.
    end method.
   
    // use when the from dataset may go out of scope (when the owner is garbage collected)
    method public static void CloneParentRelation (pNewdataset as handle,phNewChild as handle, phOriginalChild as handle):
        define variable hOriginalBuffer as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable hRelation       as handle no-undo.
      
        hRelation = phOriginalChild:parent-relation.
        if valid-handle(hRelation) then
        do:
            hOriginalBuffer = hRelation:parent-buffer.
            hbuffer = TableUtil:CreateTableLike(hOriginalBuffer):default-buffer-handle.   
            hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
            pNewdataset:add-buffer(hBuffer).
            pNewDataset:add-relation(hBuffer,phNewChild,hRelation:relation-fields,hRelation:reposition,true).
            CloneParentRelation(pNewDataset,hBuffer,hOriginalBuffer). 
        end.
        catch e as Progress.Lang.Error :
            // they have been added to a dataset and cannot be deleted
            // caller must catch and delete the dataset 
/*            if valid-handle(hBuffer) then*/
/*                delete object hBuffer.   */
            undo, throw e.     
        end catch.
        
    end method. 
   
    // use when the from dataset may go out of scope (when the owner is garbage collected)
    method public static void CloneChildRelations (pNewdataset as handle,phNewParent as handle, phOriginalParent as handle):
        define variable i as integer no-undo.
        define variable hOriginalBuffer as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable hRelation       as handle no-undo.
        define variable hNewRelation    as handle no-undo.
        do i = 1 to phOriginalParent:num-child-relations:
            hRelation       = phOriginalParent:get-child-relation (i).
            hOriginalBuffer = hRelation:child-buffer.
            hBuffer = TableUtil:CreateTableLike(hOriginalBuffer):default-buffer-handle. 
            hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
            pNewdataset:add-buffer(hBuffer).
            hNewRelation = pNewDataset:add-relation(phNewParent,hBuffer,hRelation:relation-fields,hRelation:reposition,true).
            if hRelation:recursive then 
                hNewRelation:recursive = true.
            else    
                CloneChildRelations(pNewDataset,hBuffer,hOriginalBuffer). 
        end.
        catch e as Progress.Lang.Error :
                     // they have been added to a dataset and cannot be deleted
            // caller must catch and delete the dataset 
/*            if valid-handle(hBuffer) then*/
/*                delete object hBuffer.   */
            undo, throw e.     
        end catch.
    end method. 
    
    
    method public static handle CopyChildRelation (pNewdataset as handle,phNewParent as handle, phOriginalRelation as handle):
        define variable i as integer no-undo.
        define variable hOriginalBuffer as handle no-undo.
        define variable hParentBuffer   as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable hRelation       as handle no-undo.
        hOriginalBuffer = phOriginalRelation:child-buffer.
        create buffer hBuffer for table hOriginalBuffer buffer-name hOriginalBuffer:table-handle:name.
        hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
        pNewdataset:add-buffer(hBuffer).
        hRelation = pNewDataset:add-relation(phNewParent,hBuffer,phOriginalRelation:relation-fields,phOriginalRelation:reposition,true).
        CopyChildRelations(pNewDataset,hBuffer,hOriginalBuffer). 
        return hRelation.
        catch e as Progress.Lang.Error :
            if valid-handle(hBuffer) then 
                delete object hBuffer.     
            undo, throw e.     
        end catch.
    end method. 
    
    
    method public static handle GetRecursiveRelation (phHandle as handle):
        define variable iChild as integer no-undo. 
        define variable hRelation as handle no-undo.
        if not valid-handle(phHandle) then
            undo, throw new IllegalArgumentError("Invalid handle").
            
        if phHandle:type = "dataset" then
        do iChild = 1 to phHandle:num-top-buffers :
            hRelation = GetRecursiveRelation(phHandle:get-top-buffer (iChild)).
            if valid-handle(hRelation) then
                return hRelation.                    
        end.
        else do:    
            if phHandle:type <> "buffer" then
                undo, throw new IllegalArgumentError(subst("Invalid handle type '&1'",phHandle:type)).
            do iChild = 1 to phHandle:num-child-relations:
                if phHandle:get-child-relation (iChild):recursive then 
                    return phHandle:get-child-relation (iChild).
                else do:
                    hRelation = GetRecursiveRelation(phHandle:get-child-relation (iChild):child-buffer).
                    if valid-handle(hRelation) then
                        return hRelation.   
                end.        
            end.
        end.       
        return ?.
    end method. 
    
    method public static logical HasBeforeState(phDataset as handle,piState as integer):
        define variable i as integer no-undo.
        define variable hBefore as handle no-undo.
        define variable lOk as logical no-undo.
        do i = 1 to phDataset:num-buffers:
            hBefore = phDataset:get-buffer-handle(i):before-buffer.
            if valid-handle(hBefore) then
            do on error undo, throw:
                lOk = false.
                lOk = hBefore:find-first(subst("where row-state(&1) = &2",hBefore:name,piState)) no-error.
                if lOk then 
                    return true.
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 565 then
                        return false.
                    undo, throw e.        
                end catch.
            end.      
        end.    
        return false.
        
    end method.    
    
    method public static logical HasRecords(phDataset as handle):
        define variable i as integer no-undo.
        do i = 1 to phDataset:num-buffers:
            if phDataset:get-buffer-handle(i):table-handle:has-records then 
                return true.
        end.    
        return false.
    end method.    
    
    method public static void DeepCopy(phDataset as handle,output dataset-handle phDatasetCopy):
        phDatasetCopy = phDataset.
        delete object phDataset. 
    end method.    
    
    method public static void CopyDataset(phToDataset as handle,phFromDataset as handle, pcCopyMode as character):
        define variable hToBuffer as handle no-undo.
        define variable hFromBuffer as handle no-undo.
        define variable i as integer no-undo.
        define variable cDifferentMessage as character no-undo
            init "Copy Dataset where From and To Datasets are different".
        
        if lookup(pcCopyMode,",replace,merge") = 0 then
            undo, throw new IllegalArgumentError("Copy Mode must be 'replace' or 'merge' or blank").
            
        if phFromDataset:type <> "dataset" then 
            undo, throw new IllegalArgumentError("From Dataset is not a dataset").
        
        if phToDataset:type <> "dataset" then 
            undo, throw new IllegalArgumentError("To Dataset is not a dataset").
        
        // in order to support copy of dataset same name buffers and serialize-names with different parents 
        //  we currently use number to get both to and from buffer. 
        // TODO? - use parent names and  GetBufferFromUrlQualifier instead (?) - a bit more forgiving perhaps?  
        do i = 1 to phFromDataset:num-buffers:
            hFromBuffer = phFromDataset:get-buffer-handle(i).
            hToBuffer   = phToDataset:get-buffer-handle(i).
            if hToBuffer:name <> hFromBuffer:name then 
               undo, throw new NotImplementedError(cDifferentMessage).
            
            if valid-handle(hFromBuffer:parent-relation) then 
            do:
                if not valid-handle(hToBuffer:parent-relation) then 
                    undo, throw new NotImplementedError(cDifferentMessage).
                if hFrombuffer:parent-relation:parent-buffer:name <> hTObuffer:parent-relation:parent-buffer:name then 
                    undo, throw new NotImplementedError(cDifferentMessage).
            end.   
            else if valid-handle(hToBuffer:parent-relation) then 
                undo, throw new NotImplementedError(cDifferentMessage).
            
            TableUtil:CopyTable(hToBuffer, hFromBuffer, pcCopyMode).
        end.
            
        do i = 1 to phFromDataset:num-buffers:
            hFromBuffer = phFromDataset:get-buffer-handle(i).
            // this relies on the validation loop above
            hToBuffer   = phToDataset:get-buffer-handle(i).
            
            TableUtil:CopyTable(hToBuffer, hFromBuffer, pcCopyMode).
        end.          
    end method.    
    
    
/*    method public static void AssertAndEnforceForeignKeys(phDataset as handle):      */
/*        define variable i       as integer no-undo.                                  */
/*        define variable hBuffer as handle no-undo.                                   */
/*        Assert:IsType(phDataset, DataTypeEnum:Dataset).                              */
/*        do i = 1 to phDataset:num-buffers:                                           */
/*            hbuffer = phDataset:get-buffer-handle(i).                                */
/*            AssertAndEnforceForeignKeysBuffer(hBuffer).                              */
/*        end.                                                                         */
/*    end method.                                                                      */
/*                                                                                     */
/*    method private static void AssertAndEnforceForeignKeysBuffer(phBuffer as handle):*/
/*        define variable i             as integer   no-undo.                          */
/*        define variable hChildBuffer  as handle    no-undo.                          */
/*        define variable hQuery        as handle no-undo.                             */
/*        Assert:IsType(phBuffer, DataTypeEnum:Buffer).                                */
/*        if valid-object(phBuffer:parent-relation) then                               */
/*        do:                                                                          */
/*            message phBuffer:parent-relation:query:prepare-string                    */
/*            phBuffer:parent-relation:nested                                          */
/*            phBuffer:parent-relation:foreign-key-hidden                              */
/*            hQuery:add-buffer(phBuffer).                                             */
/*            hQuery:query-prepare(subst("for each &1",phbuffer:name)).                */
/*            hQuery:query-open().                                                     */
/*            hQuery:get-first.                                                        */
/*            do while phBuffer:avail:                                                 */
/*                hQuery:get-next.                                                     */
/*            end.                                                                     */
/*        end.                                                                         */
/*    end.                                                                             */
/*                                                                                     */
    
         
end class.
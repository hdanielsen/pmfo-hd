 
 /*------------------------------------------------------------------------
    File        : SaveUtil
    Purpose     : A utility that is used instead of save-row-changes for conflict check and 
                  save tt the attached/mapped field values when some of the datasource key fields are not 
                  mapped/attached, but managed by the ProxyManager or SourceFieldManager thus preventing use of 
                  save-row-changes
                - The posssible evolution of this is to merge all of these and stop 
                  using save-row-changes altogether (but we need to attach for read
                  for performance and save of very many rfecords and/orlarge tables 
                  also benfits froma attahc and seve-roe-changex so this is not certain )
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Nov 04 13:40:55 EDT 2021
    Notes       : This is needed also on Create because the ProxyManager and SourceFieldManager
                  need to have the db table available since the row is not available after save 
                  AND we cannot use findSource since  the keys woild not be assigned by 
                  save-row-changes        
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.IFieldManager from propath.
 
class Pmfo.Core.DataLayer.Util.SaveUtil // implements IFieldManager
    : 
  
    constructor public SaveUtil (  ):
        super ().
    end constructor.
    
    
/*    // shoul;d not be called hewre                                                           */
/*    define public property HasKey as logical get.                                            */
/*                                                                                             */
/*    method public character WhereExpression(phDbBuffer as handle, pcSourcename as character):*/
/*        return "".                                                                           */
/*    end method.                                                                              */
/*                                                                                             */
    
    method public character CheckRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle):
        define variable cChanges as character no-undo.
        ProcessRow(true,pUpdateTarget,phBeforeBuffer,output cchanges).
        return cChanges. 
    end method.   
    
    method public logical SaveRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle): 
        define variable cDummy as character no-undo.
        return ProcessRow(false,pUpdateTarget,phBeforeBuffer,output cDummy).
    end method.      
    
    method protected logical ProcessRow(plCheck as logical, pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle, output pcchanges as character):
    
        define variable cFieldMap as character no-undo.
        define variable i as integer no-undo.
        define variable iNumEntries as integer no-undo.
        define variable hField as handle no-undo.
        define variable hBeforeField as handle no-undo.
        
        define variable cField as character no-undo.
        define variable cDbField as character no-undo.
        define variable hDbField as handle no-undo.
        define variable hDbBuffer as handle no-undo.
        define variable hAfterBuffer as handle no-undo.
        define variable iStartExtent as integer no-undo.
        define variable iNumExtents as integer no-undo.
        define variable iArrayPos as integer no-undo.
        define variable iDBExtent as integer no-undo.
        define variable cChanges as character no-undo.
        define variable lProcessed as logical no-undo.
        define variable iFieldExtent as integer no-undo.
        
        if phBeforeBuffer:row-state = row-modified 
        or (plCheck = false and phBeforeBuffer:row-state = row-created)
        then  
        do:
            hDBBuffer = pUpdateTarget:BufferHandle.
            hAfterBuffer = phBeforeBuffer:after-buffer.
            
            if phBeforeBuffer:row-state = row-modified then
                pUpdateTarget:FindSource(phBeforeBuffer,pUpdateTarget:BufferHandle,exclusive-lock).
            else do:  
                
                pUpdateTarget:BufferHandle:buffer-create.
                
            end.
            cFieldMap   = hAfterBuffer:data-source-complete-map.
            iNumEntries = num-entries(cFieldMap).
            do i = 1 to INumEntries by 2:
                assign
                    cField        = entry(i,cFieldMap) 
                    cDbField      = entry(i + 1,cFieldMap)
                    iArrayPos     = index(cDbField,"[")
                    .
                if iArrayPos > 0 then
                do:
                    if iArrayPos > 0 then
                    do:
                        iDBExtent = integer(right-trim(substring(cDbField,iArrayPos + 1),"]")).
                        cDbField = substring(cDbField,1,iArrayPos - 1).
                    end.   
                end.
                else iDbExtent = 0. 
                
                assign 
                    hField        = hAfterBuffer:buffer-field(entry(2,cField,"."))
                    hDbField      = hDBBuffer:buffer-field(entry(2,cDbField,"."))
                    .  
                
                iStartExtent = if hField:extent > 0 then 1 else 0.
                iNumExtents  = hField:extent.
                if phBeforeBuffer:row-state = row-modified then
                   hBeforeField = phBeforeBuffer:buffer-field(hField:name).
                
                do iFieldExtent = iStartExtent to iNumExtents:
                    if phBeforeBuffer:row-state = row-created
                    or hField:buffer-value(iFieldExtent) <> hBeforeField:buffer-value(iFieldExtent)
                    then
                    do:
                        if plcheck then
                        do:
                             pcchanges = pcchanges
                                       + (if pcchanges = "" then "" else ",")
                                       + hField:name.
                        end.
                        else  
                            hdbField:buffer-value(iDbExtent) = hField:buffer-value(iFieldExtent).
                        lProcessed = true. // NOTE true means ok saved or if check bad difference
                    end.
                end.
            end.
            return lProcessed. 
        end.    
    end method.     
    
    
    
end class.
 
 /*------------------------------------------------------------------------
    File        : LookupDS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 23 11:37:26 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.DataSource.
using Pmfo.Core.Common.INameService from propath.

block-level on error undo, throw.

class Pmfo.Repository.DataSource.LookupTypeDS abstract inherits DataSource: 
    &scop mode protected
    {Pmfo/Repository/schema/lookupType.i reference-only}
    &undefine mode
    
    define protected abstract property NameService as INameService no-undo get.
     
    define override public property EntityName as character init "lookupTypes" no-undo get. protected set.
        
    constructor protected LookupTypeDS ( input phDbBuffer as handle ):
        super (input phDbBuffer).
    end constructor.
    
    method void Bind(input table ttLookupType bind):
    end method.
    
    method override protected void PrepareBuffer( input pBuffer as handle ):
        define variable hTable as handle no-undo.
        pBuffer:set-callback("After-Row-fill","AfterRow").
        hTable = pBuffer:table-handle.
        Bind (table-handle hTable bind).        
    end method.

    method override protected void PrepareSaveBuffer( input pBuffer as handle ):
        define variable hTable as handle no-undo.
        define variable hMeasure as handle no-undo.
        define variable hHTMeasure as handle no-undo.
        hTable = pBuffer:table-handle.
        Bind (table-handle hTable bind).        
    end method.
    
    method public override void AfterRow(input dataset-handle phDataset ):
        assign
            ttLookupType.className   = NameService:ClassName(ttLookupType.TypeCode,true) 
            ttLookupType.modelType   = lc(substr(ttLookupType.className,1,1)) + substr(ttLookupType.className,2)   
            ttLookupType.entityName  = NameService:PluralizeName(lc(substr(ttLookupType.className,1,1)) + substr(ttLookupType.className,2)). 
    end method.
    
end class.
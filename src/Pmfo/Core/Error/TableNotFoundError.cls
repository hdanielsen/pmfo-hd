 
 /*------------------------------------------------------------------------
    File        : TableRequestNotFoundError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 11 17:42:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.BusinessLogic.Parser.KeyValueParser from propath.

class Pmfo.Core.Error.TableNotFoundError inherits NotFoundError: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public TableNotFoundError (phBuffer as handle, pTableRequest as IGetDataTableRequest, pError as Error):
        super (input GetMessage(pTableRequest,phbuffer), input pError).
    end constructor.

    constructor public TableNotFoundError (phBuffer as handle, pQueryEntry as IQueryEntry, pError as Error):
        super (input GetMessage(pQueryEntry,phbuffer), input pError).
    end constructor.
    
    constructor public TableNotFoundError (phBuffer as handle, pExpression as character, pError as Error):
        super (input GetMessage(pExpression,phbuffer), input pError).
    end constructor.
    
    constructor public TableNotFoundError (phBuffer as handle, pTableRequest as IGetDataTableRequest):
        super (input GetMessage(pTableRequest,phbuffer)).
    end constructor.

    constructor public TableNotFoundError (phBuffer as handle, pQueryEntry as IQueryEntry):
        super (input GetMessage(pQueryEntry,phbuffer)).
    end constructor.
    
    constructor public TableNotFoundError (phBuffer as handle, pExpression as character):
        super (input GetMessage(pExpression,phbuffer)).
    end constructor.
    
    constructor public TableNotFoundError (phBuffer as handle, pExpression as character, plHasPublicNames as logical):
        super (input GetMessage(pExpression,plHasPublicnames,phbuffer)).
    end constructor.
    
    constructor public TableNotFoundError (phBuffer as handle, pExpression as character, plHasPublicNames as logical, peError as Error):
        super (input GetMessage(pExpression,plHasPublicnames,phbuffer),peError).
    end constructor.
    
    method protected character GetMessage(pTableRequest as IGetDataTableRequest, phBuffer as handle):
        define variable oQueryEntry    as IQueryEntry no-undo.
        if valid-object(pTableRequest:QueryDefinition) then
        do:
            oQueryEntry = pTableRequest:QueryDefinition:QuerySelection.
            if valid-object(oQueryEntry) then
                return GetMessage(oQueryEntry,phBuffer).
        end.
        return BufferUtil:GetRowName(phBuffer,true).                        
    end method.     
    
    method protected character GetMessage(pcWhereString as character, phBuffer as handle):
        return GetMessage(pcWhereString,false,phbuffer).     
    end method.    
      
    method protected character GetMessage(pcWhereString as character, plHasPublicNames as logical, phBuffer as handle):
        define variable oKeyValueParser as KeyValueParser no-undo.
        
        pcWhereString = left-trim(pcWhereString).
        if pcWhereString begins "where " then
             pcWhereString = substr(pcWhereString,7).
        oKeyValueParser = new KeyValueParser().
            // use the version that support single unamed key 
        return GetMessage(oKeyValueParser:GetKeyEntry (pcWhereString,phBuffer,plHasPublicNames,false),phbuffer).     
         
    end method.    
     
    
    method protected character GetMessage(pQueryEntry as IQueryEntry, phBuffer as handle):
        define variable cRowName    as character no-undo.
        define variable cExpression as character no-undo.
        if type-of(pQueryEntry,Pmfo.Core.BusinessLogic.IQueryEntry) then
        do:
            cExpression = cast(pQueryEntry,Pmfo.Core.BusinessLogic.IQueryEntry):ToString(phBuffer,true).
             
            if cExpression begins "(" then
                cExpression = StringUtil:ExtractParenthesis(cExpression,true).
        end. 
        else     
             cExpression = pQueryEntry:ToString().
             
        return subst("&1 where &2",BufferUtil:GetRowName(phBuffer,true),cExpression).
   end method.

end class.
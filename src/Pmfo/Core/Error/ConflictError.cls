 
 /*------------------------------------------------------------------------
    File        : ConflictError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 30 20:07:06 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using OpenEdge.Core.StringConstant from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Progress.Lang.Error from propath.
using Pmfo.Util.StringUtil from propath.

class Pmfo.Core.Error.ConflictError inherits ApplicationError: 
    
    define variable MsgTemplate  as character init "&1 has been changed by another user and cannot be updated. Please refresh the data and try again." no-undo. 
    define variable MsgTemplateFlds as character init "&1 has &2 to &3 by another user. Please refresh the data and try again." no-undo. 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ConflictError (pTableName as character):
        super (subst(MsgTemplate,pTableName)).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
    
     constructor public ConflictError (pTableName as character,pcChanges as character):
        super (AddChangesToMessage(pTableName,pcChanges)).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
    
    // Error with table and where clause for the buffer (typically before buffer but works on any temp-table buffer)
    constructor public ConflictError (phBeforebuffer as handle):
        super (subst(MsgTemplate,GetWhereClause(phBeforeBuffer))).
    end constructor.
    
     // Error with table and where clause for the buffer (typically before buffer but works on any temp-table buffer and additional info about the change. 
     // If the changes has LF the text is added after the standard message otherwise it is assumed to be a list of fields that woll be substitued to MsgTemplateFlds
    constructor public ConflictError (phBeforebuffer as handle, pcChanges as character):
        super (AddChangesToMessage(phBeforeBuffer,pcChanges)).
    end constructor.
    
    // manage changes parameter for constructor  
    method protected character AddChangesToMessage(phBeforebuffer as handle,pcchanges as character):
        if index(pcChanges,StringConstant:LF) > 0 then 
            return subst(MsgTemplate,GetWhereClause(phBeforeBuffer)) + right-trim(pcChanges,StringConstant:LF) .  
        else 
            return subst(MsgTemplateFlds,
                         GetWhereClause(phBeforeBuffer),
                         if num-entries(pcCHanges) > 1 then "recent conflicting changes" else "a recent conflicting change",
                         if index(pcChanges," ") > 0 then pcChanges else StringUtil:GetMessageFromEntries(pcChanges) 
                         ).
    end method.    
   
    // manage changes parameter for constructor  
    method protected character AddChangesToMessage(pcTablename as character,pcchanges as character):
        if index(pcChanges,StringConstant:LF) > 0 then 
            return subst(subst(MsgTemplate,pcTablename)) + right-trim(pcChanges,StringConstant:LF) .  
        else 
            return subst(MsgTemplateFlds,
                         pcTablename,
                         if num-entries(pcCHanges) > 1 then "recent conflicting changes" else "a recent conflicting change",
                         if index(pcChanges," ") > 0 then pcChanges else StringUtil:GetMessageFromEntries(pcChanges) 
                         ).
    end method.
     
    constructor public ConflictError (phBeforebuffer as handle,phQueryentry as IQueryEntry):
        super (subst(MsgTemplate,GetWhereClause(phBeforeBuffer,phQueryentry))).
    end constructor.
   
    // the inner error os likely not very useful, but if needed the constructor is here
    constructor public ConflictError (pTableName as character, pError as Error):
        super (subst(MsgTemplate,pTableName),pError).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
    
    method protected character GetWhereClause(phBuffer as handle,pQueryEntry as IQueryEntry):
        define variable cKeyExpression as character no-undo.
        define variable cMsg as character no-undo.
        
        if phBuffer:avail then
        do:
            cKeyExpression = pQueryEntry:ToString(phBuffer,true).
            if cKeyExpression begins "(" and substr(cKeyExpression,length(cKeyExpression),1) = ")" then 
               cKeyExpression = substr(cKeyExpression,2,length(cKeyExpression) - 2).
                
            cMsg = subst("&1 where &2",BufferUtil:GetRowName(phBuffer,true),cKeyExpression).
        end.
        else 
            cmsg = BufferUtil:GetRowName(phBuffer,true).
        return cMsg. 
    end method.     
    
    // Get an error text with table and where clause for the buffer (typically before buffer but works on any temp-table buffer)
    method protected character GetWhereClause(phBuffer as handle):
        define variable pQueryEntry as IQueryEntry no-undo.
        define variable cMsg as character no-undo.
        if phBuffer:avail then
        do:
            pQueryEntry = cast(QueryUtil:GetCurrentRowKeyEntry(phBuffer),Pmfo.Core.BusinessLogic.IQueryEntry).
            return GetWhereClause(phBuffer,pQueryEntry).
        end.
        else 
            return BufferUtil:GetRowName(phBuffer,true).
         
    end method.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Conflict.
    end method. 
end class.
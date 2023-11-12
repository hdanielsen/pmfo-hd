 
 /*------------------------------------------------------------------------
    File        : TableConflictError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 10/27/2022
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

class Pmfo.Core.Error.TableConflictError inherits ApplicationError: 
    
    define variable MsgTemplate  as character init "The posted &1data conflicts with the stored data. &2. Please refresh the data and try again." no-undo. 
    
    constructor public TableConflictError (pcTableInfo as character, pcRecordInfo as character):
        
        super (subst(MsgTemplate,trim(pcTableInfo) + " ",pcRecordInfo)).
    end constructor.
   
    constructor public TableConflictError (pcRecordInfo as character):
        super (subst("&1. Please refresh the data and try again.",right-trim(pcRecordInfo,"."))).
    end constructor.
   
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Conflict.
    end method. 
end class.
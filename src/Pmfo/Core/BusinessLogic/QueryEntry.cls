
/*------------------------------------------------------------------------
    File        : QueryEntry
    Purpose     : Allow generic cast to set Join
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 24 12:59:33 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Ccs.BusinessLogic.JoinEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.BusinessLogic.QueryEntry abstract implements  IQueryEntry:  
    define public property Join         as JoinEnum          no-undo 
        get():
            if not valid-object(this-object:Join) then
                return JoinEnum:None. 
            return this-object:Join.    
        end. 
        set.
        
    method public abstract character ToString(phBuffer as handle,plUseSerializeName as logical).
    method public abstract character ToString(phBuffer as handle,plUseSerializeName as logical, plTrimParenthesis as logical).
    
    method public override logical Equals(pObject as Object):
        define variable lEquals as logical no-undo.
        lEquals = super:Equals(pObject).
        if lEquals = false then
        do:
           if type-of(pObject,IQueryEntry) then 
               lEquals = pObject:ToString() = ToString().
        
        end.
        return lEquals. 
    end.    
         
end class.
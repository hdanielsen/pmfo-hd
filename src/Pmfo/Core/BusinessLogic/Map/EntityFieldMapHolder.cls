 
 /*------------------------------------------------------------------------
    File        : EntityFieldMapHolder
    Purpose     : Cache of public to internal field maps for many entities
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Oct 08 06:52:06 EDT 2021
    Notes       : Used during build of internal requests from client requests.
                  The main purpose of the class is to mao pulbiv name ot internal name
                  while avoiding performing loops through buffer fields everytime
                  this information is needed.  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Util.BufferUtil from propath.
using OpenEdge.DataAdmin.Error.IllegalArgumentError from propath.

class Pmfo.Core.BusinessLogic.Map.EntityFieldMapHolder: 
    
    // map of child buffers
    define protected property FieldMapMap as IStringStringMap no-undo get. protected set.
    
    // map of main buffer (could be stored in the map above, but the vast majority of uses only need the main map )
    define protected property FieldMap    as character        no-undo get. protected set.
    
    method public character GetFieldMap(phBuffer as handle):
        
        define variable cFieldMap as character no-undo.
        if not valid-handle(phbuffer) then
            undo, throw new IllegalArgumentError("Invalid buffer handle").
        if valid-object(FieldMapMap) then 
        do:
            cFieldMap = FieldMapMap:Get(phBuffer:name).
        end.
        else 
            FieldMapMap = new StringStringMap().
            
        if cFieldMap = "" or cFieldMap = ?  then
        do: 
            cFieldMap = BufferUtil:GetPublicToNameFieldPairs(phBuffer).
            FieldMapMap:put(phBuffer:name,cFieldMap). 
        end.     
        return cFieldMap.
    end. 
            
end class.
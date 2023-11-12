 
 /*------------------------------------------------------------------------
    File        : ClassModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 09 15:20:45 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.Set from propath.
using OpenEdge.Core.StringConstant from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.ClassModel abstract: 
    define public property Name           as character no-undo get. protected set.
    define public property InheritsString as character no-undo get. protected set.
    
    define protected temp-table ttUsing no-undo
         field name as char
         index name as primary unique name.
    
    define protected temp-table ttInclude no-undo
         field name as char
         index name as primary unique name.
         
    define public property Methods        as ICollection no-undo 
        get():
            if not valid-object(Methods) then 
            do:
                Methods = new Set().
            end.
            return Methods.    
        end. 
        set.
    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ClassModel (pcName as char, pcInherits as char):
        super ().
        Name = pcName.
        if pcInherits > "" then 
        do:
            AddUsing(pcInherits).
            InheritsString = entry(num-entries(pcInherits,"."),pcInherits,".").
        end.
    end constructor.
    
    method public character extent GetUsing():
        define variable i as integer no-undo.
        define variable cUsing as character extent no-undo.   
        for each ttUsing:
            i = i + 1.
        end.
        extent(cUsing) = i.
        i = 0.
        for each ttUsing by ttusing.name:
            i = i + 1.
            cUsing[i] = "using " + ttUsing.name + " from propath.".
        end.
        return cUsing.
    end method.      
    
    method public logical AddUsing(pcName as char):
        if not can-find(ttUsing where ttUsing.name = pcName) then
        do:
            create ttUsing.
            ttUsing.name = pcName.
            return true.
        end.
        return false.
    end method.      
    
    method public logical AddInclude(pcName as char):
        if not can-find(ttInclude where ttInclude.name = pcName) then
        do:
            create ttInclude.
            ttInclude.name = pcName.
            return true.
        end.
        return false.
    end method.      
    
    method public character extent GetIncludes():
        define variable i as integer no-undo.
        define variable cIncludes as character extent no-undo.   
        for each ttInclude:
            i = i + 1.
        end.
        extent(cIncludes) = i.
        for each ttInclude:
            cIncludes[i] = ttInclude.name.
        end.
        return cIncludes.
    end method.      
             
end class.
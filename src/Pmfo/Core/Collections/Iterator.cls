 /************************************************
Copyright (c) 2023 by Progress Software Corporation. All rights reserved.
*************************************************/
 /*------------------------------------------------------------------------
    File        : Iterator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue May 09 07:34:00 EDT 2023
    Notes       : Extend corelib to manage primitive types 
                  Integer and character has PMFO holders to be able to with correct value 
                  (corelib classes use int64 and longchar)
                  to be able to be used interchangeably in reflection and overload 
                  correectly language 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.Iterator from propath.
using OpenEdge.Core.DateHolder from propath.
using OpenEdge.Core.DateTimeHolder from propath.
using OpenEdge.Core.DateTimeTzHolder from propath.
using OpenEdge.Core.LogicalValue from propath.
using OpenEdge.Core.WidgetHandle from propath.
using Pmfo.Core.Common.CharacterHolder from propath.
using Pmfo.Core.Common.IntegerHolder from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.Collections.Iterator inherits Iterator: 

    constructor public Iterator ( input poCol as OpenEdge.Core.Collections.ICollection, input tt as handle, input ofield as character ):
        super (input poCol, input tt, input ofield).
    end constructor.

    constructor public Iterator ( input poCol as OpenEdge.Core.Collections.ICollection, input tt as handle, input ofield as character, input sortfield as character ):
        super (input poCol, input tt, input ofield, input sortfield).
    end constructor.

    constructor public Iterator ( input poCol as OpenEdge.Core.Collections.ICollection, input tt as handle, input ofield as character, input sortfield as character, input querystring as character ):
        super (input poCol, input tt, input ofield, input sortfield, input querystring).
    end constructor.
    
    method public override Object Next(  ):
        case ObjectFieldHandle:data-type:
            when  "character" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new CharacterHolder(ObjectFieldHandle:buffer-value()).
            end. 
            when  "integer" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new IntegerHolder(ObjectFieldHandle:buffer-value()).
            end. 
            when  "handle" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new WidgetHandle(ObjectFieldHandle:buffer-value()).
            end.
            when  "logical" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new LogicalValue(ObjectFieldHandle:buffer-value()).
            end.
            when  "date" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new DateHolder(ObjectFieldHandle:buffer-value()).
            end.
            when  "datetime" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new DateTimeHolder(ObjectFieldHandle:buffer-value()).
            end.
            when  "datatime-tz" then 
            do:
                QueryHandle:get-next().
                if bufferHandle:avail then 
                    return new DateTimeTzHolder(ObjectFieldHandle:buffer-value()).
            end.
             
            otherwise // Object, decimnal and int64 is managed correctly 
                return super:Next().
        end.     
        return ?. 
    end method.
end class.
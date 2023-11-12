 
 /*------------------------------------------------------------------------
    File        : IntegerKeySet
    Purpose     : Set with primitive integers for primituve map with integer keys  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed May 10 15:07:50 EDT 2023
    Notes       : Invoked from primitiveMap.i entryset when key is integer   
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Support.IIntegerHolder from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Collections.IIntegerSet from propath.
using Pmfo.Core.Collections.KeySet from propath.

class Pmfo.Core.Collections.IntegerKeySet inherits KeySet implements IIntegerSet: 
    
    constructor public IntegerKeySet ( input pMap as OpenEdge.Core.Collections.IMap, input pTT as handle, input pQueryString as character, input pValueField as character ):
        super (input pMap, input pTT, input pQueryString, input pValueField).
    end constructor.

    constructor public IntegerKeySet ( input poMap as OpenEdge.Core.Collections.IMap, input phTT as handle, input pcKeyField as character ):
        super (input poMap, input phTT, input pcKeyField).
    end constructor.


    /* ToArray should not be used with large collections
       If there is too much data the ABL will throw:
       Attempt to update data exceeding 32000. (12371) */
    method public integer extent ToIntegerArray():
        define variable i as integer no-undo.
        define variable iKeys as integer extent no-undo.
        define variable iterator as IIterator no-undo.
        
        if Size eq 0 then
            return iKeys.
            
        extent(iKeys) = Size.
        iterator = Iterator().
        do while iterator:hasNext():
           i = i + 1.
           IKeys[i] = cast(iterator:Next(),IIntegerHolder):Value.
        end.
        return IKeys.
    end method.
end class.
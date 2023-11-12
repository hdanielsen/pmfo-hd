
/*------------------------------------------------------------------------
    File        : IColumnColumnMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Apr 07 09:33:49 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.IMap.

interface Pmfo.Core.Common.IColumnColumnMap inherits IMap:  
    /** Adds an entry to the mape
        
        @param character The key value
        @param character The value
        @return character The value added (may be previous value) */
    method public character Put(input pcKey as character, input pcValue as character).
    
    /** Retrieves the value for a particular key
    
        @param character The key value
        @return character The associated value */
    method public character Get(input pcKey as character).
    
    /** Removes the value for a particular key
    
        @param character The key value
        @return character The associated value */
    method public character Remove(input pcKey as character).
    
    /** Indicates whether a map exists for this key

        @param character the key value
        @return logical True if this key exists */
    method public logical ContainsKey(input pcKey as character).

    /** Indicates whether there is at least one value represented
        by the parameter in the map.
        
        @param character The value
        @return logical True if there is at least one entry with this value */    
    method public logical ContainsValue(input pcValue as character).
     
end interface.
/************************************************/
/*------------------------------------------------------------------------
    File        : ICharacterCharacterMap 
    Purpose     : A typed CharacterHolder/CharacterHolder Map 
    Syntax      : 
    Description : 
    Author(s)   : pjudge
    Created     : Wed Dec 18 13:58:44 EST 2013
    Notes       : contains IMap methods using ABL primitives instead 
                  of OpenEdge.Core.String  
  ----------------------------------------------------------------------*/

using Ccs.Common.Support.ICharacterHolder from propath.
using Pmfo.Core.Collections.ICharacterCharacterMap from propath.

interface Pmfo.Core.Collections.ICharacterCharacterMap inherits OpenEdge.Core.Collections.IMap
    : 
    /** Adds an entry to the mape
        @param character The key value
        @param longchar The value
        @return longchar The value added (may be previous value) */
    method public character Put(input pcKey as character, input pcValue as character).
    
    /** Retrieves the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public character Get(input pcKey as character).
    
    /** Removes the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public character Remove(input pcKey as character).
    
    /** Indicates whether a map exists for this key

        @param character the key value
        @return logical True if this key exists */
    method public logical ContainsKey(input pcKey as character).

    /** Indicates whether there is at least one value represented
        by the parameter in the map.
        
        @param longchar The value
        @return logical True if there is at least one entry with this value */    
    method public logical ContainsValue(input pcValue as character).
    
    method public ICharacterHolder Put(input poKey as ICharacterHolder, input poValue as ICharacterHolder).

    /** Removes the mapping for this key from this map if it is present (optional operation).*/
    method public ICharacterHolder Remove(input poKey as ICharacterHolder).
    
    /** Returns true if this map contains a mapping for the specified key. */    
    method public logical ContainsKey(input poKey as ICharacterHolder).
    
    /** Returns true if this map maps one or more keys to the specified value.*/
    method public logical ContainsValue(input poValue as ICharacterHolder).

    /** Returns the value to which this map maps the specified key.*/
    method public ICharacterHolder Get(input poKey as ICharacterHolder).
    
    /** Adds all data from the input map into this map */    
    method public void PutAll(input poMap as ICharacterCharacterMap).

end interface.

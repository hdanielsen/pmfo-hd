/************************************************/
/*------------------------------------------------------------------------
    File        : IPrimitiveMapInterface.i  
    Purpose     : A typed Map 
    Syntax      : 
    Description : 
    Author(s)   : pjudge - (corelib version)
    Created     : Wed Dec 18 13:58:44 EST 2013
    Notes       : contains IMap methods using ABL primitives instead 
                  of OpenEdge.Core.String  
----------------------------------------------------------------------*/
 

interface Pmfo.Core.Collections.{&interfacename} inherits OpenEdge.Core.Collections.IMap
    : 
    /** Adds an entry to the mape
        @param character The key value
        @param longchar The value
        @return longchar The value added (may be previous value) */
    method public {&valuetype} Put(input pKey as {&keytype}, input pValue as {&valuetype}).
    
    /** Retrieves the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public {&valuetype} Get(input pKey as {&keytype}).
    
    /** Removes the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public {&valuetype} Remove(input pKey as {&keytype}).
    
    /** Indicates whether a map exists for this key

        @param character the key value
        @return logical True if this key exists */
    method public logical ContainsKey(input pKey as {&keytype}).

    /** Indicates whether there is at least one value represented
        by the parameter in the map.
        
        @param longchar The value
        @return logical True if there is at least one entry with this value */    
    method public logical ContainsValue(input poValue as {&valuetype}).
    
    method public {&valueinterface} Put(input poKey as {&keyinterface}, input poValue as {&valueinterface}).

    /** Removes the mapping for this key from this map if it is present (optional operation).*/
    method public {&valueinterface} Remove(input poKey as {&keyinterface}).
    
    /** Returns true if this map contains a mapping for the specified key. */    
    method public logical ContainsKey(input poKey as {&keyinterface}).
    
    /** Returns true if this map maps one or more keys to the specified value.*/
    method public logical ContainsValue(input poValue as {&valueinterface}).

    /** Returns the value to which this map maps the specified key.*/
    method public {&valueinterface} Get(input poKey as {&keyinterface}).
    
    /** Adds all data from the input map into this map */    
    method public void PutAll(input poMap as Pmfo.Core.Collections.{&interfacename}).

end interface.

 /*------------------------------------------------------------------------
    File        : PrimitiveMap.i
    Purpose     : Map of character values 
    Author(s)   : hdaniels
    Created     : Fri Apr 21 17:34:16 EDT 2023
  ----------------------------------------------------------------------*/


block-level on error undo, throw.


class Pmfo.Core.Collections.{&classname} implements {&interface}:
    
    // The bucket in which we hold all the Maps for this type. There's only ever one (since its static) 
    // so this class is responsible for clenaing up after itself
    define static private temp-table PrimitiveMap no-undo
        field ParentMap as int64
        field Key as {&keytype}
        field Val as {&valuetype}
       // field ValHash as raw
      //  field ValObj as Progress.Lang.Object    // CharacterHolder
        index idxParent as primary ParentMap    // This index ensures that the default record order is 'as-added' and not by Key  
        index idxKey as unique ParentMap Key
        index idxVal ParentMap val  
        .
    /* Returns the number of key-value mappings in this map.*/
    define public property Size as integer no-undo get. protected set.
    
    /* A collection of only the values in the Map */
    define public property Values as OpenEdge.Core.Collections.ICollection no-undo 
    get():
         return new Pmfo.Core.Collections.ValueCollection(this-object, temp-table PrimitiveMap:handle, substitute('ParentMap eq &1':u, int64(this-object)), "Val").
    end.
    
    /* An  ordered set of only the keys in the Map */
    define public property KeySet as OpenEdge.Core.Collections.ISet no-undo 
    get():
        /* Return a new KeySet on each request. This is somewhat
           inefficient, but doing so prevents a circular reference from being created.
           
           This property is typically used in a transient fashion (ie for iteration
           over the contents of the Map) and is expected to be scoped to a small block
           like a single method. */
           &if "{&keytype}" = "integer" &then  
         return new Pmfo.Core.Collections.IntegerKeySet(this-object, temp-table PrimitiveMap:handle, substitute('ParentMap eq &1':u, int64(this-object)), "Key").
           &else
        return new Pmfo.Core.Collections.KeySet(this-object, temp-table PrimitiveMap:handle, substitute('ParentMap eq &1':u, int64(this-object)), "Key").
           &endif     
       
    end.
    
    /* An ordered set of key-value objects in the Map */
    define public property EntrySet as OpenEdge.Core.Collections.ISet no-undo 
    get():
        /* Return a new EntrySet on each request. This is somewhat
           inefficient, but doing so prevents a circular reference from being created.
           
           This property is typically used in a transient fashion (ie for iteration
           over the contents of the Map) and is expected to be scoped to a small block
           like a single method. */
        return new Pmfo.Core.Collections.EntrySet(this-object, temp-table PrimitiveMap:handle, substitute('ParentMap eq &1':u, int64(this-object)), "Key").
    end.
     
    /* Destructor */
    destructor {&classname}():
        this-object:Clear().
    end destructor.
    
    /* Default constructor */
    constructor public {&classname}():
        super().
    end constructor.
    
    /* Removes all mappings from this map (optional operation). */
    method public void Clear():
        define buffer bMap for PrimitiveMap.
        
        for each bMap
           where bMap.ParentMap eq int64(this-object):
            delete bMap.
        end.
        assign this-object:Size = 0.
    end method.
    
    /* Returns true if this map contains no key-value mappings.
    
       @return logical TRUE if the map has no entries; FALSE otherwise */
    method public logical IsEmpty():
        return (this-object:Size eq 0).
    end method.
    
     /* Returns true if this map contains a mapping for the specified key. 
       
       @param Object A key value. Must be valid and of type OE.Core.String
       @return logical TRUE of the map contains an entry with the specified key; FALSE otherwise */
    method public logical ContainsKey(input pKey as Progress.Lang.Object):
        OpenEdge.Core.Assert:IsType(pKey, get-class({&keyinterface})).
        
        return ContainsKey(cast(pKey, {&keyinterface})).
    end method. 
    
    /* Returns true if this map contains a mapping for the specified key. 
       
       @param String A key value. Must be valid
       @return logical TRUE of the map contains an entry with the specified key; FALSE otherwise */
    method public logical ContainsKey(input pKey as {&keyinterface}):
        define buffer bMap for PrimitiveMap.
        
        OpenEdge.Core.Assert:NotNull(pKey, 'Map key').
        
        FindMapRecord(pKey:Value, buffer bMap).
        return (available bMap).
    end method.
    
    method public logical ContainsAllValues(input pValues as OpenEdge.Core.Collections.ICollection):
        define variable iter as OpenEdge.Core.Collections.IIterator no-undo.
        define variable item as Progress.Lang.Object no-undo.
        
        OpenEdge.Core.Assert:NotNull(pValues, 'Check values').
        
        /* if the 'check' collection is empty, then true */  
        if pValues:IsEmpty() then
            return true.
        
        /* if the passed in collection is larger than this collection,
           this cannot be true. */
        if pValues:Size gt this-object:Size then
            return false.
        
        /* one or both collections has data */
        assign iter = pValues:Iterator().
        do while iter:HasNext():
            assign item = iter:Next().
            
            // the ContainsValue() method throws errors if the item is not a valid interface object.
            // we just want to return false
            if  valid-object(item)
            and not type-of(item, {&valueinterface})
            then
                return false.
            // null items are supported
            if not this-object:ContainsValue(item) then
                return false.
        end.
        
        return true.
    end method.
    
    /* Returns true if this map contains all of the keys in a collection.
       
       @param ICollection A collection of keys. Must be a valid object.
       @return logical TRUE if all the keys in the input collection are in this map */
    method public logical ContainsAllKeys(input pKeys as OpenEdge.Core.Collections.ICollection):
        define variable iter as OpenEdge.Core.Collections.IIterator no-undo.
        define variable item as Progress.Lang.Object no-undo.
        
        OpenEdge.Core.Assert:NotNull(pKeys, 'Keys collection').
        
        // if the 'check' collection is empty, then true   
        if pKeys:IsEmpty() then
            return true.
        
        // if there are fewer items in this collection than the source, this one can't have them all 
        if this-object:Size lt pKeys:Size then
            return false.
        
        assign iter = pKeys:Iterator().
        do while iter:HasNext():
            assign item = iter:Next().
            // the ContainsKey() method throws errors on these conditions;
            // we just want to return false
            if not valid-object(item) then
                return false.
            
            if not this-object:ContainsKey(item) then
                return false.
        end.
        
        return true.
    end method.
    
    /* Returns true if this map contains all of the values in a collection.
       
       @param ICollection A collection of values. Must be a valid object.
       @return logical TRUE if all the values in the input collection are in this map */
    method public logical ContainsValue(input pValue as Progress.Lang.Object):
        define variable vNull as {&valuetype} no-undo.
        if valid-object(pValue) then
        do:
            OpenEdge.Core.Assert:IsType(pValue, get-class({&valueinterface})).
            return ContainsValue(cast(pValue, {&valueinterface})).
        end.
        else
            return ContainsValue(vNull).
    end method.
    
    /* Returns TRUS if there's at least one value in the map that equals the input value.
       
       @param String The value to find. May be null.
       @return logical TRUE if at least one value exists in the map; FALSE otherwise. */
    method public logical ContainsValue(input pValue as {&valueinterface}):
        define buffer bMap for PrimitiveMap.
        
        
        FindMapRecordByVal(pValue:Value, buffer bMap).
        return (available bMap).
    end method.
    
    /* Adds a value and/or key to the map.
       
       @param Object The key for the map entry. Must be a valid value and must be of type OpenEdge.Core.String, but may have an empty or unknown value. Is unique in the map.
       @param Object The value matching the key. If it is a valid value, it must be of type OpenEdge.Core.String, but may have an empty or unknown value. 
       @return String The previous value, or unknown if none. If a valid value, will be of type OpenEdge.Core.String. Unknown is a valid value too. */
    method public Progress.Lang.Object Put(input pKey as Progress.Lang.Object,
                             input pValue as Progress.Lang.Object):
        OpenEdge.Core.Assert:IsType(pKey,   get-class({&keyinterface})).
        if valid-object(pValue) then
            OpenEdge.Core.Assert:IsType(pValue, get-class({&valueinterface})).
        
        return this-object:Put(pKey, cast(pValue, {&valueinterface})).
    end method.
    
     /* Adds a value and/or key to the map.
       
       @param String The key for the map entry. Must be a valid value, but may have an empty or unknown value. Is unique in the map.
       @param String The value matching the key. Must be a valid value, but may have an empty or unknown value. 
       @return String The previous value, or unknown if none. Unknown is a valid value too. */
    method public {&valueinterface} Put(input pKey   as {&keyinterface},
                                       input pValue as {&valueinterface}):
        OpenEdge.Core.Assert:NotNull(pKey, 'Map key').
        
        return this-object:Put(pKey:Value, pValue).
    end method.
    
    /* Adds a value and/or key to the map.
       Does the actual addition into the temp table.
       
       @param character The key for the map entry. May be empty or unknown. Is unique in the map.
       @param String The value matching the key. May be empty or unknown.
       @return String The previous value, or unknown if none. Unknown is a valid value too. */
    method protected {&valueinterface} Put(input pcKey  as {&keytype},
                                           input pValue as {&valueinterface}):
        define variable oldValue as {&valueinterface} no-undo.
        define buffer bMap for PrimitiveMap.
        
        FindMapRecord(pcKey, buffer bMap).
        if not available bMap then
        do:
            create bMap.
            assign bMap.ParentMap = int64(this-object)
                   bMap.Key       = pcKey
                   oldValue       = ?
                   Size           = Size + 1
                   .
        end.
        else
            assign oldValue = new {&valueclass}(bMap.Val).
        
        assign bMap.Val = pValue:Value.
        
        return oldValue.
    end method.
    
    /* Adds a value and/or key to the map.
       
       @param character The key for the map entry. May be empty or unknown. Is unique in the map.
       @param character The value matching the key. May be empty or unknown.
       @return character The previous value, or unknown if none. Unknown is a valid value too. */
    method public {&valuetype} Put(input pcKey as {&keytype}, input pcValue as {&valuetype}).
        define variable oldValue as {&valuetype} no-undo.
        define buffer bMap for PrimitiveMap.
        
        FindMapRecord(pcKey, buffer bMap).
        if not available bMap then
        do:
            create bMap.
            assign bMap.ParentMap = int64(this-object)
                   bMap.Key       = pcKey
                   oldValue       = ?
                   Size           = Size + 1
                   .
        end.
        else
            assign oldValue = bMap.Val.
        
        assign bMap.Val = pcValue.
        
        return oldValue.
    end method.
    
    /* Copies all of the mappings from the specified map to this map (optional operation).
       
       @param IMap A valid map */
    method public void PutAll(input pMap as OpenEdge.Core.Collections.IMap):
        define variable mapEntry as OpenEdge.Core.Collections.IMapEntry no-undo.
        define variable iter  as OpenEdge.Core.Collections.IIterator no-undo.
        
        OpenEdge.Core.Assert:NotNull(pMap, 'Source map').
        
        if type-of(pMap, {&interface}) then
            PutAll(cast(pMap, {&interface})).
        else
        do:
            assign iter = pMap:EntrySet:Iterator(). 
            do while iter:HasNext():
                assign mapEntry = cast(iter:Next(), OpenEdge.Core.Collections.IMapEntry).
                this-object:Put(mapEntry:Key, mapEntry:Value).
            end.
        end.
    end method.
    
     /* Adds all the values from an input map
       
       @param IStringStringMap A valid map */
    method public void PutAll(input pMap as {&interface}):
        define variable mapEntry as OpenEdge.Core.Collections.IMapEntry no-undo.
        define variable iter  as OpenEdge.Core.Collections.IIterator no-undo.
        define buffer srcMap for PrimitiveMap.
        define buffer tgtMap for PrimitiveMap.
        
        OpenEdge.Core.Assert:NotNull(pMap, 'Source map').
        
        if type-of(pMap, Pmfo.Core.Collections.{&classname}) then
        do:
            for each srcMap
               where srcMap.ParentMap eq int64(pMap):
                
                FindMapRecord(srcMap.Key, buffer tgtMap).
                if not available tgtMap then
                do:
                    create tgtMap.
                    assign tgtMap.ParentMap = int64(this-object)
                           tgtMap.Key       = srcMap.Key
                           Size             = Size + 1
                           .
                end.
                assign tgtMap.Val = srcMap.Val.
            end.
        end.
        else
        do:
            assign iter = pMap:KeySet:Iterator(). 
            do while iter:HasNext():
                assign mapEntry = cast(iter:Next(), OpenEdge.Core.Collections.IMapEntry).
                // safe cast because this is a IStringStringMap
                this-object:Put(mapEntry:Key, cast(mapEntry:Value, {&valueinterface})).
            end.
        end.
    end method.
    
     /** Retrieves the value for a particular key
        
        @param Object The key value. The value object must be valid and of type OpenEdge.Core.String
        @return Object The associated value, or unknown if the entry does not exist. If the value exists, it will be of type OpenEdge.Core.String */
    method public Progress.Lang.Object Get(input pKey as Progress.Lang.Object):
        OpenEdge.Core.Assert:IsType(pKey, get-class({&valueinterface})).
        
        return Get(cast(pKey, {&valueinterface})).
    end method.
    
    /** Retrieves the value for a particular key
        
        @param String The key value. The value object must be valid.
        @return String The associated value, or unknown if the entry does not exist */
    method public {&valueinterface} Get(input pKey as {&keyinterface}):
        define buffer bMap for PrimitiveMap.
        define variable vVal as {&valuetype} no-undo.
        OpenEdge.Core.Assert:NotNull(pKey, 'Map key').
        vVal = Get(pKey:Value).
        if vVal <> ? then 
            return new {&valueclass}(vVal).
        else
            return ?.
    end method.
    
    /** Retrieves the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public {&valuetype} Get(input pcKey as {&keytype}).
        define buffer bMap for PrimitiveMap.
        
        FindMapRecord(pcKey, buffer bMap).
        if available bMap then
            return bMap.Val.
        else
            return ?.
    end method.
    
    /** Removes the value for a particular key
    
        @param Object The key value. Must be a valid object, and must be of type OpenEdge.Core.String
        @return Object The associated value (of type OpenEdge.Core.String), or unknown if there is no entry. */
    method public Progress.Lang.Object Remove(input pKey as Progress.Lang.Object):
        OpenEdge.Core.Assert:IsType(pKey, get-class({&keyinterface})).
        return Remove(cast(pKey, {&keyinterface})).
    end method.
    
    /** Removes the value for a particular key
    
        @param String The key value. Must be a valid object.
        @return String The associated value, or unknown if there is no entry. */
    method public {&valueinterface} Remove(input pKey as {&keyinterface}):
        define variable oldVal as {&valueinterface} no-undo.
        define buffer bMap for PrimitiveMap.
        
        OpenEdge.Core.Assert:NotNull(pKey, 'Object map').
        
        FindMapRecord(pKey:value, buffer bMap).
        if available bMap then
        do:
            assign oldVal = new {&valueclass}(bMap.Val)
                   Size   = Size - 1
                   .
            delete bMap.
            return oldVal.
        end.
        else
            return ?.
    end method.
    
    /** Removes the value for a particular key
    
        @param character The key value
        @return longchar The associated value */
    method public {&valuetype} Remove(input pcKey as {&keytype}).
        &if "{&valuetype}" = "handle" &then
            define variable oldVal as {&valuetype} no-undo.
        &else 
            define variable oldVal as {&valuetype} init ? no-undo.
        &endif    
        define buffer bMap for PrimitiveMap.
        
        OpenEdge.Core.Assert:NotNull(pcKey,'{&keytype} Key').
        
        FindMapRecord(pcKey, buffer bMap).
        if available bMap then
        do:
            assign oldVal = bMap.Val
                   Size   = Size - 1
                   .
            delete bMap.
        end.
        return oldVal.
    end method.
    
     /** Removes the mappings for all key from this map if it is present (optional operation).
        
        @param ICollection A collection of keys to remove */
    method public void RemoveAll(input pKeys as OpenEdge.Core.Collections.ICollection):
        define variable key as {&keyinterface} no-undo.
        define variable iter  as OpenEdge.Core.Collections.IIterator no-undo.
        define buffer bMap for PrimitiveMap.
        
        OpenEdge.Core.Assert:NotNull(pKeys, 'Keys collection').
        
        assign iter = pKeys:Iterator(). 
        do while iter:HasNext():
            assign key = cast(iter:Next(),{&keyinterface}).
            if FindMapRecord(key:Value, buffer bMap) then
            do:
               assign Size = Size - 1.
               delete bMap.
           end.
        end.
    end method.
    
    /** Indicates whether a map exists for this key

        @param character the key value
        @return logical True if this key exists */
    method public logical ContainsKey(input pcKey as {&keytype}).
        define buffer bMap for PrimitiveMap.
        
        FindMapRecord(pcKey, buffer bMap).
        return (available bMap).
    end method.
    /** Indicates whether there is at least one value represented
        by the parameter in the map.
        
        @param longchar The value
        @return logical True if there is at least one entry with this value */    
    method public logical ContainsValue(input pcValue as {&valuetype}).
        define buffer bMap for PrimitiveMap.
        
        FindMapRecordByVal(pcValue, buffer bMap).
        return (available bMap).
    end method.
    
     /* Helper method to find a map entry by key
       
       @param character The key value to find
       @param buffer for PrimitiveMap
       @return logical TRUE if a record exists with the given key; FALSE if there are no matching entries */
    method private logical FindMapRecord(input pKey as {&keytype}, buffer pMap for PrimitiveMap):
        release pMap.
        
        if can-find(pMap
              where pMap.ParentMap eq int64(this-object)
                and pMap.Key eq pKey                    )
        then
            find pMap
           where pMap.ParentMap eq int64(this-object)
             and pMap.Key eq pKey no-error.
        
        return (available pMap).
    end method.
    
    /* Helper method to find a map entry by value
       
       @param raw The hash value of the value being sought
       @param buffer for PrimitiveMap
       @return logical TRUE if at least one record exists with that value; FALSE if there are no matching entries */
    method private logical FindMapRecordByVal(input pVal as {&valuetype}, buffer pMap for PrimitiveMap):
        release pMap.
        
        if can-find(first pMap
              where pMap.ParentMap eq int64(this-object)
                and pMap.Val       eq pVal               )
        then
            find first pMap
           where pMap.ParentMap eq int64(this-object)
             and pMap.Val   eq pVal no-error.
        
        return (available pMap).
    end method.
    
    
end class.
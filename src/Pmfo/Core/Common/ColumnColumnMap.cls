 
 /*------------------------------------------------------------------------
    File        : ColumnColumnMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Apr 07 08:16:09 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.IMap.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.Set from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.MapEntry from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Common.IColumnColumnMap from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.ColumnColumnMap implements IColumnColumnMap: 
    define private variable KeyList   as character no-undo.
    define private variable ValueList as character no-undo.
    
    define public property EntrySet as ISet no-undo    
        get().
             define variable i as   integer no-undo.
             define variable cKey   as character no-undo.
             define variable cValue as character no-undo. 
             define variable oEntrySet as ISet no-undo.
             define variable oMapentry as IMapEntry no-undo.
             oEntrySet = new Set().
              
             do i = 1 to num-entries(this-object:KeyList):
                 cKey   = entry(i,KeyList).
                 cValue = entry(i,ValueList).
                 oMapEntry = new MapEntry(this-object,new String(cKey)).
                 oMapEntry:Value = new String(cValue).
                 oEntrySet:Add(oMapEntry).    
             end.
             return oEntrySet.
        end.     
         

    define public property KeySet as ISet no-undo
        get().
             define variable i as   integer no-undo.
             define variable cKey   as character no-undo.
             define variable oKeySet as ISet no-undo.
             oKeySet = new Set().
              
             do i = 1 to num-entries(this-object:KeyList):
                 cKey   = entry(i,KeyList).
                 oKeySet:Add(new String(cKey)).    
             end.
             return oKeySet.
        end.     
         

    define public property Size as integer no-undo 
        get():
            return num-entries(KeyList).
        end.
        set. 

    define public property Values as ICollection no-undo 
        get().
             define variable i       as integer no-undo.
             define variable cValue  as character no-undo.
             define variable oValues as ICollection no-undo.
             oValues = new Collection().
              
             do i = 1 to num-entries(this-object:ValueList):
                 cValue  = entry(i,ValueList).
                 oValues:Add(new String(cValue)).    
             end.
             return oValues.
        end.     
        set. 

    method public void Clear(  ):
        KeyList = "".
        ValueList = "".
    end method.

    method public logical ContainsAllKeys( input poKeys as ICollection ):
        define variable i     as integer   no-undo.
        define variable oIter as IIterator no-undo.
        define variable lContains as logical no-undo.
        define variable oIterator as IIterator.
        define variable oCheckObject as Object no-undo.
        
        /* if both are empty collections then true */        
        lContains = poKeys:IsEmpty() and this-object:IsEmpty().
        
        /* if the passed in collection is larger than this collection,
           this cannot be true. */        
        lContains = poKeys:size le this-object:size.
        
        /* one or both collections has data */
        if not lContains then
        do:
            oIterator = poKeys:Iterator().
        
            do while oIterator:HasNext():
                oCheckObject = oIterator:Next().
                lContains = this-object:ContainsKey(oCheckObject).
                if not lContains then
                    leave.
            end.
        end.
        
        return lContains.

    end method.

    method public logical ContainsAllValues( input poValues as ICollection ):
        define variable i     as integer   no-undo.
        define variable oIter as IIterator no-undo.
        define variable lContains as logical no-undo.
        define variable oIterator as IIterator.
        define variable oCheckObject as Object no-undo.
        
        /* if both are empty collections then true */        
        lContains = poValues:IsEmpty() and this-object:IsEmpty().
        
        /* if the passed in collection is larger than this collection,
           this cannot be true. */        
        lContains = poValues:size le this-object:size.
        
        /* one or both collections has data */
        if not lContains then
        do:
            oIterator = poValues:Iterator().
        
            do while oIterator:HasNext():
                oCheckObject = oIterator:Next().
                lContains = this-object:ContainsValue(oCheckObject).
                if not lContains then
                    leave.
            end.
        end.
        
        return lContains.
    end method.

    method public logical ContainsKey( input poKey as Object ):
        define variable cKey as character no-undo.
        if type-of(poKey,String) then 
        do:
            cKey = string(cast(poKey,String):Value).
            return ContainsKey(cKey). 
        end.      
        return false.
        catch e as Progress.Lang.Error :
            return false.    
        end catch.
    end method.
    
    method public logical ContainsKey( input pcKey as character ):
        return lookup(pcKey,KeyList) > 0. 
    end method.
    
    method public logical ContainsValue( input poValue as Object ):
        define variable cValue as character no-undo.
        if type-of(poValue,String) then 
        do:
            cValue = string(cast(poValue,String):Value).
            return ContainsValue(cValue). 
        end.      
        return false.
        catch e as Progress.Lang.Error :
            return false.    
        end catch.
   
    end method.

    method public logical ContainsValue( input pcValue as character ):
        return lookup(pcValue,ValueList) > 0.
    end method.

    method public Object Get( input poKey as Object ):
        define variable cKey as character no-undo.
        define variable i as integer no-undo.
        if type-of(poKey,String) then 
        do:
            cKey = cast(poKey,String):Value.
            return new String(Get(cKey)).
        end.
        return ?.
    end method.
    
    method public character Get( input pcKey as character ):
        define variable i as integer no-undo.
        i = lookup(pcKey,KeyList).
        if i > 0 then 
            return entry(i,ValueList).
        return ?.    
    end method.
    

    method public logical IsEmpty(  ):
        return KeyList = "".
    end method.

    method public Object Put( input poKey as Object, input poValue as Object ):
        define variable oOld as Object no-undo.
        define variable cKey   as character no-undo.
        define variable cValue as character no-undo.
        oOld = Get(poKey).
        Assert:IsType(poKey, get-class(String)).
        Assert:IsType(poValue, get-class(String)).
        cKey = cast(poKey,String):Value.
        cValue = cast(poKey,String):Value.
        return new String(this-object:Put(cKey,cValue)).
    end method.
    
    method public character Put( input pcKey as character, input pcValue as character ):
        define variable cOld as character no-undo.
        define variable iPos as integer no-undo.
        Assert:NotNullOrEmpty(pcKey,"Key column cannot be null or empty").
        Assert:NotNullOrEmpty(pcValue,"Value column cannot be null or empty").
/*        /* 65 is max table name and max fieldname - may increase to  */                                                         */
/*        if length(pcKey) > 65 then                                                                                              */
/*            undo, throw new IllegalArgumentError("Max length for a key column is 65. Use IStringStringMap for large strings").  */
/*        if length(pcValue) > 65 then                                                                                            */
/*            undo, throw new IllegalArgumentError("Max length for a value column is 65. Use IStringStringMap for large strings").*/
/*                                                                                                                                */
        iPos = lookup(pcKey,KeyList).
        if iPos > 0 then
        do:
            cOld = entry(iPos,ValueList).
            entry(iPos,ValueList) = pcValue.
            return cold.
        end.
        else do:    
            KeyList   = KeyList 
                      + (if KeyList = "" then "" else ",")
                      + pcKey.
            ValueList = ValueList
                      + (if Size = 1 then "" else ",")
                      + pcValue.
            return ?. 
        end.          
    end method.
    
    /* Adds all entries from another map to this one.
       
       @param IMap The input map */
    method public void PutAll(poMap as IMap):
        define variable oKey as Object no-undo.
        define variable oIter as IIterator no-undo.    
 
        oIter = poMap:KeySet:Iterator(). 
        do while oIter:hasNext():
            oKey = oIter:Next().
            this-object:Put(oKey,poMap:Get(oKey)).
        end.
    end method.
    
    method public Object Remove( input poKey as Object ):
        undo, throw new NotImplementedError("Remove"). 
    end method.
    
    method public character Remove( input poKey as character ):
        undo, throw new NotImplementedError("Remove"). 
    end method.
    
    method public void RemoveAll( input poKeys as ICollection ):
        undo, throw new NotImplementedError("RemoveAll"). 
    end method.

end class.
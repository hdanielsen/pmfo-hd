
/*------------------------------------------------------------------------
    File        : NamedArrays
    Purpose     : Allow passing of multiple arrays to a single method
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Sep 30 08:52:14 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.DateArrayHolder from propath.
using OpenEdge.Core.DateTimeArrayHolder from propath.
using OpenEdge.Core.DateTimeTzArrayHolder from propath.
using OpenEdge.Core.DecimalArrayHolder from propath.
using OpenEdge.Core.LogicalArrayHolder from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.Common.CharacterArrayHolder from propath.
using Pmfo.Core.Common.INamedArrayHolder from propath.
using Pmfo.Core.Common.IntegerArrayHolder from propath.
using Pmfo.Core.Common.NamedArrayNameEnum from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.Common.NamedArrayHolder implements INamedArrayHolder:
    
    define public property NameQualifier as NamedArrayNameEnum no-undo get. protected set.
    
    define protected property HolderMap as IMap no-undo 
        get():
           if not valid-object(HolderMap) then
               HolderMap = new Map().
           return HolderMap.         
        end.
        set.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public NamedArrayHolder (pNameQualifer as  NamedArrayNameEnum ):
        super ().
        NameQualifier = pNameQualifer.
    end constructor.
       
    method public logical AddValues(pcName as char, pValues as character extent). 
        return AddValues(pcName,new CharacterArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as integer extent). 
        return AddValues(pcName,new IntegerArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as int64 extent). 
        return AddValues(pcName,new OpenEdge.Core.IntegerArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as decimal extent). 
        return AddValues(pcName,new DecimalArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as logical extent). 
        return AddValues(pcName,new LogicalArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as date extent). 
        return AddValues(pcName,new DateArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as datetime extent). 
        return AddValues(pcName,new DateTimeArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pValues as datetime-tz extent). 
        return AddValues(pcName,new DateTimeTzArrayHolder(pValues)).
    end method.
    
    method public logical AddValues(pcName as char, pholder as IPrimitiveArrayHolder). 
        define variable iSize as integer no-undo.
        define variable oOld  as Object no-undo.
        iSize = HolderMap:Size.
        oOld = HolderMap:Put(new String(pcName), pholder).
        return valid-object(oold) or HolderMap:Size > iSize.  
    end method.
   
    {Pmfo/Core/Common/getArrayValuesMethod.i Character Ccs.Common.Support.ICharacterArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i Integer   Ccs.Common.Support.IIntegerArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i Int64     Ccs.Common.Support.IInt64ArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i Decimal   Ccs.Common.Support.IDecimalArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i Logical   Ccs.Common.Support.ILogicalArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i Date      Ccs.Common.Support.IDateArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i DateTime  Ccs.Common.Support.IDateTimeArrayHolder}
    {Pmfo/Core/Common/getArrayValuesMethod.i DateTime-TZ Ccs.Common.Support.IDateTimeTzArrayHolder GetDateTimeTzValues}
    
    method public IPrimitiveArrayHolder GetHolder(pcName as char):
        return cast(HolderMap:Get(new String(pcName)),IPrimitiveArrayHolder).
    end method.    
    
    method public character GetBufferQualifier(phBuffer as handle): 
        case NameQualifier:
            when NamedArrayNameEnum:Public then
                return phBuffer:table-handle:serialize-name.
            when NamedArrayNameEnum:Table then
                return phBuffer:table-handle:name.
            when NamedArrayNameEnum:Buffer then
                return phBuffer:name.
            when NamedArrayNameEnum:BufferSerializeName then
                return phBuffer:serialize-name.
            otherwise   
               undo, throw new UnsupportedOperationError(subst("Get Qualifer for buffer for NamedArrayNameEnum:&1",
                                                                NameQualifier:ToString()
                                                               )
                                                         ).
        end case. 
    end method.
    
    method public IPrimitiveArrayHolder extent GetBufferEntries(phBuffer as handle): 
        define variable cQualifier             as character no-undo. 
        define variable oKeys                  as Object extent no-undo.
        define variable oPrimitiveArrayHolders as IPrimitiveArrayHolder extent no-undo. 
        define variable i                      as integer no-undo.
        define variable cName                  as character no-undo.
        
        cQualifier = GetBufferQualifier(phBuffer).
        oKeys = HolderMap:KeySet:ToArray().
        do i = 1 to extent(oKeys):
            cName = cast(oKeys[i],String):Value.
            if cName begins cQualifier + "." then 
            do:
               extent(oPrimitiveArrayHolders) = (if extent(oPrimitiveArrayHolders) = ? then 0 else extent(oPrimitiveArrayHolders)) + 1.
               oPrimitiveArrayHolders[extent(oPrimitiveArrayHolders)] = cast(HolderMap:Get(oKeys[i]),IPrimitiveArrayHolder).
            end.
        end.  
        return oPrimitiveArrayHolders.
    end method.    
    
    method public character extent GetNames():
        define variable oKeys as Object extent no-undo. 
        define variable i as integer no-undo.
        define variable cNames as character extent no-undo.
        oKeys = HolderMap:KeySet:ToArray().
        extent(cNames) = extent(oKeys).
        do i = 1 to extent(cNames):
            cNames[i] = cast(oKeys[i],String):Value.
        end.  
        return cNames.  
    end method.    
    
    method public IPrimitiveArrayHolder GetHolderForField(hField as handle):
       define variable cQualifier as character no-undo.
       define variable cName      as character no-undo.
       define variable cFieldname as character no-undo.
       cQualifier = GetBufferQualifier(hField:buffer-handle).
       case NameQualifier: 
           when NamedArrayNameEnum:BufferSerializeName 
           or when NamedArrayNameEnum:Public then 
               cFieldname = hField:serialize-name.
           otherwise
               cFieldname = hField:name.
           
       end.    
       return GetHolder(subst("&1.&2",cQualifier,cFieldname)).   
    
    end method.
    
    method public character extent GetNamesForBuffer(phBuffer as handle):
        define variable oKeys as Object extent no-undo. 
        define variable i as integer no-undo.
        define variable cNames as character extent no-undo.
        define variable cQualifier as character no-undo.
        oKeys = HolderMap:KeySet:ToArray().
        do i = 1 to extent(oKeys):
            cQualifier = GetBufferQualifier(phBuffer).
            if cast(oKeys[i],String):Value begins cQualifier + "." then
            do:
                extent(cNames) = (if extent(cNames) = ? then 0 else extent(cNames)) + 1.
                cNames[extent(cNames)] = cast(oKeys[i],String):Value.
            end.    
        end.  
        return cNames.  
    end method.  
        
end class.
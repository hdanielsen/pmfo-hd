 
 /*------------------------------------------------------------------------
    File        : PrimitiveHolderUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jun 27 09:06:51 EDT 2020
    Notes       : get primitive value from IPrimitiveHolder
                  get named primitive value from extent with 
                  character name and IPrimitiveValue value properties       
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Support.ICharacterArrayHolder from propath.
using Ccs.Common.Support.ICharacterHolder from propath.
using Ccs.Common.Support.IDateArrayHolder from propath.
using Ccs.Common.Support.IDateHolder from propath.
using Ccs.Common.Support.IDateTimeArrayHolder from propath.
using Ccs.Common.Support.IDateTimeHolder from propath.
using Ccs.Common.Support.IDateTimeTzArrayHolder from propath.
using Ccs.Common.Support.IDateTimeTzHolder from propath.
using Ccs.Common.Support.IDecimalArrayHolder from propath.
using Ccs.Common.Support.IDecimalHolder from propath.
using Ccs.Common.Support.IInt64ArrayHolder from propath.
using Ccs.Common.Support.IInt64Holder from propath.
using Ccs.Common.Support.IIntegerArrayHolder from propath.
using Ccs.Common.Support.IIntegerHolder from propath.
using Ccs.Common.Support.ILogicalArrayHolder from propath.
using Ccs.Common.Support.ILogicalHolder from propath.
using Ccs.Common.Support.ILongcharArrayHolder from propath.
using Ccs.Common.Support.ILongcharHolder from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using OpenEdge.Core.Integer from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.OutOfBoundsError from propath.
 
class Pmfo.Util.PrimitiveHolderUtil:
    
    method public static IPrimitiveHolder GetValue(pcName as character,pObject as Object extent):
        define variable i as integer no-undo.
        define variable cName as character no-undo.
        define variable oHolder as IPrimitiveHolder no-undo.
        do i = 1 to extent(pObject):
            cName = dynamic-property(pObject[i],"name").
            if cName = pcName then
                return dynamic-property(pObject[i],"value").
        end.   
        undo, throw new IllegalArgumentError(subst("&1 is not a valid Name in &2 array instance",pcName,pObject[i]:GetClass():TypeName)). 
    end method.
    
    method public static IPrimitiveArrayHolder GetValues(pcName as character,pObject as Object extent):
        define variable i as integer no-undo.
        define variable cName as character no-undo.
        define variable oHolder as IPrimitiveArrayHolder no-undo.
        do i = 1 to extent(pObject):
            cName = dynamic-property(pObject[i],"name").
            if cName = pcName then
                return dynamic-property(pObject[i],"value").
        end.   
        undo, throw new IllegalArgumentError(subst("&1 is not a valid Name in &2 array instance",pcName,pObject[i]:GetClass():TypeName)). 
    end method.
    
    method public static character GetCharacterValue(pholder as IPrimitiveHolder):
        if type-of(pHolder,String) then
            return string(cast(pholder,String):value).
        return cast(pholder,ICharacterHolder):value.
    end. 
    
    method public static longchar GetLongcharValue(pholder as IPrimitiveHolder):
        return cast(pholder,ILongcharHolder):value.
    end. 
    
    method public static integer GetIntegerValue(pholder as IPrimitiveHolder):
        if type-of(pHolder,Integer) then
            return integer(cast(pholder,Integer):value).
        return cast(pholder,IIntegerHolder):value.
    end.    
    
    method public static int64 GetInt64Value(pholder as IPrimitiveHolder):
        return cast(pholder,IInt64Holder):value.
    end.    
     
    method public static logical GetLogicalValue(pholder as IPrimitiveHolder):
        return cast(pholder,ILogicalHolder):value.
    end.    
     
    method public static decimal GetDecimalValue(pholder as IPrimitiveHolder):
        return cast(pholder,IDecimalHolder):value.
    end.    
    
    method public static date GetDateValue(pholder as IPrimitiveHolder):
        return cast(pholder,IDateHolder):value.
    end.    
     
    method public static datetime GetDatetimeValue(pholder as IPrimitiveHolder):
        return cast(pholder,IDateTimeHolder):value.
    end.    
    
    method public static datetime-tz GetDatetimetzValue(pholder as IPrimitiveHolder):
        return cast(pholder,IDateTimeTzHolder):value.
    end.    
 
    method public static character GetCharacterValue(pcname as character,pObject as Object extent):
        return GetCharacterValue(GetValue(pcname,pObject)).
    end.
    
    method public static longchar GetLongcharValue(pcname as character,pObject as Object extent):
        return GetLongcharValue(GetValue(pcname,pObject)).
    end. 
    
    method public static integer GetIntegerValue(pcname as character,pObject as Object extent):
        return GetIntegerValue(GetValue(pcname,pObject)).
    end.
    
    method public static int64 GetInt64Value(pcname as character,pObject as Object extent):
        return GetInt64Value(GetValue(pcname,pObject)).
    end.
       
    method public static logical GetLogicalValue(pcname as character,pObject as Object extent):
        return GetLogicalValue(GetValue(pcname,pObject)).
    end.
    
    method public static decimal GetDecimalValue(pcname as character,pObject as Object extent):
        return GetDecimalValue(GetValue(pcname,pObject)).
    end.
    
    method public static date GetDateValue(pcname as character,pObject as Object extent):
        return GetDateValue(GetValue(pcname,pObject)).
    end.
    
    method public static datetime GetDatetimeValue(pcname as character,pObject as Object extent):
        return GetDatetimeValue(GetValue(pcname,pObject)).
    end.
    
    method public static datetime-tz GetDatetimetzValue(pcname as character,pObject as Object extent):
        return GetDatetimetzValue(GetValue(pcname,pObject)).
    end.
    
    // extents 
    method public static integer extent GetIntegerValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IIntegerArrayHolder) then
        do:
            return cast(pholder,IIntegerArrayHolder):value.
        end.
    end.  
    
    method public static int64 extent GetInt64Values(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IInt64ArrayHolder) then
        do:
            return cast(pholder,IInt64ArrayHolder):value.
        end.
    end.  
    
    method public static decimal extent GetDecimalValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IDecimalArrayHolder) then
        do:
            return cast(pholder,IDecimalArrayHolder):value.
        end.
        
    end. 
    
    method public static logical extent GetLogicalValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,ILogicalArrayHolder) then
        do:
            return cast(pholder,ILogicalArrayHolder):value.
        end.
    end. 
    
    method public static date extent GetDateValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IDateArrayHolder) then
        do:
            return cast(pholder,IDateArrayHolder):value.
        end.
    end. 
    
    method public static datetime extent GetDateTimeValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IDateTimeArrayHolder) then
        do:
            return cast(pholder,IDateTimeArrayHolder):value.
        end.
    end. 
    
     method public static datetime-tz extent GetDateTimeTZValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,IDateTimeTzArrayHolder) then
        do:
            return cast(pholder,IDateTimeTzArrayHolder):value.
        end.
    end. 
    
    method public static character extent GetCharacterValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,ICharacterArrayHolder) then
        do:
            return cast(pholder,ICharacterArrayHolder):value.
        end.
    end.
    
    method public static longchar extent GetLongcharValues(pholder as IPrimitiveArrayHolder):
        if type-of(pHolder,ILongcharArrayHolder) then
        do:
            return cast(pholder,ILongcharArrayHolder):value.
        end.
    end.
    
    
    
    method public static character GetDataType(pholder as IPrimitiveHolder):
        
        if //type-of(pHolder,String) or - 
        // we have added Pmfo.Core.Comon.CharacterHolder for this - to avoid the amibuity with using 
        // String whihc is ILongcharHolder 
        type-of(pHolder,ICharacterHolder) then
            return "character".
        // the Pmfo.Core.Comon.IntegerHolder is implemented and used so this can work for reflection purposes
        // Openedge.Core.Integer is i?Int64holder
        if type-of(pholder,IIntegerHolder) then
            return "integer".
        if type-of(pHolder,IInt64Holder) then
            return "int64".
        if type-of(pholder,IDecimalHolder) then
            return "decimal".
        if type-of(pHolder,ILogicalHolder) then
            return "logical".
        if type-of(pHolder,IDateHolder) then
            return "date".
        if type-of(pHolder,IDateTimeHolder) then
            return "datetime".
        if type-of(pHolder,IDateTimeTzHolder) then
            return "datetime-tz".
        if type-of(pHolder,ILongcharHolder) then 
            return "longchar".
            
        undo, throw new UnsupportedOperationError(subst("GetDataType for IPrimitiveHolder of type &1",pholder:GetClass():TypeName)).     
    end method.
    
    method public static character GetDataType(pholder as IPrimitiveArrayHolder):
        define variable cType as character no-undo.
        
        case true:
            when type-of(pHolder,ICharacterArrayHolder) then
                cType =  "character".
            // the Pmfo.Core.Comon.IntegerHolder is implemented and used so this can work for reflection purposes
            // Openedge.Core.Integer is i?Int64holder
            when type-of(pHolder,IIntegerArrayHolder)then
                cType =  "integer".
            when type-of(pholder,IInt64ArrayHolder)  then
                cType =  "int64".
            when type-of(pholder,IDecimalArrayHolder) then
                cType =  "decimal".
            when type-of(pholder,ILogicalArrayHolder) then
                cType =  "logical".
            when type-of(pholder,IDateArrayHolder) then
                cType =  "date".
            when type-of(pholder,IDateTimeArrayHolder) then
                cType =  "datetime".
            when type-of(pholder,IDateTimeTzArrayHolder)then
                cType =  "datetime-tz".
            when type-of(pHolder,ILongcharArrayHolder) then 
                cType =  "longchar".
            otherwise     
                undo, throw new UnsupportedOperationError(subst("GetDataType for IPrimitiveArrayHolder of type &1",pholder:GetClass():TypeName)).
        end.
        return cType + " extent".         
    end method.
    
    method public static integer GetNumExtents(pholder as IPrimitiveArrayHolder):
        define variable cChars       as character extent no-undo.
        define variable iInts        as integer extent no-undo.
        define variable iInt64s      as int64 extent no-undo.
        define variable dDecs        as decimal extent no-undo.
        define variable lLogs        as logical extent no-undo.
        define variable dDates       as date extent no-undo.
        define variable dDatetimes   as datetime extent no-undo.
        define variable dDatetimeTzs as datetime-tz extent no-undo.
        define variable cLongs       as longchar extent no-undo.
        case true:
            // NOTE  12.7 allows the extent(xxx:Value) directly
            
            when type-of(pHolder,ICharacterArrayHolder) then
            do:
                cChars = GetCharacterValues(pholder). 
                return extent(cChars).
            end.
            when type-of(pHolder,IIntegerArrayHolder) then
            do:
                iInts = GetIntegerValues(pholder). 
                return extent(iInts).
            end.
            when type-of(pholder,IInt64ArrayHolder)  then
            do:
                iInt64s = GetInt64Values(pholder).
                return extent(iInt64s) .
            end.
            when type-of(pholder,IDecimalArrayHolder) then
            do:
                dDecs = GetDecimalValues(pholder).
                return extent(dDecs) .
            end.
            when type-of(pholder,ILogicalArrayHolder) then
            do:
                lLogs = GetLogicalValues(pholder).
                return extent(lLogs) .
            end.
            when type-of(pholder,IDateArrayHolder) then
            do:
                dDates = GetDateValues(pholder).
                return extent(dDates) .
            end.    
            when type-of(pholder,IDateTimeArrayHolder) then
            do:
                dDatetimes = GetDateTimeValues(pholder).
                return extent(dDatetimes) .
            end.    
            when type-of(pholder,IDateTimeTzArrayHolder) then
            do:
                dDatetimeTzs = GetDateTimeTZValues(pholder).
                return extent(dDatetimeTzs) .
            end.
            when type-of(pHolder,ILongcharArrayHolder) then
            do: 
                cLongs = GetLongcharValues(pholder).
                return extent(cLongs) .
            end.
            otherwise     
                undo, throw new NotImplementedError(subst("GetNumExtents for IPrimitiveArrayHolder of type &1",pholder:GetClass():TypeName)).
        end.
                  
    end method.
    
    // return one of the extent values as iPrimitiveHolder
    method public static IPrimitiveHolder GetExtentValue(pholder as IPrimitiveArrayHolder, piNum as integer):
        if piNum > GetNumExtents(pHolder) then 
           undo, throw new OutOfBoundsError("Array",piNum).
           
        case true:
            when type-of(pHolder,ICharacterArrayHolder) then
                return GetHolder(cast(pholder,ICharacterArrayHolder):value[piNum]).
                 
            // the Pmfo.Core.Comon.IntegerHolder is implemented and used so this can work for reflection purposes
            // Openedge.Core.Integer is i?Int64holder
            when type-of(pHolder,IIntegerArrayHolder)then
                return GetHolder(cast(pholder,IIntegerArrayHolder):value[piNum]).
            when type-of(pholder,IInt64ArrayHolder)  then
                return GetHolder(cast(pholder,IInt64ArrayHolder):value[piNum]).
            when type-of(pholder,IDecimalArrayHolder) then
                return GetHolder(cast(pholder,IDecimalArrayHolder):value[piNum]).
            when type-of(pholder,ILogicalArrayHolder) then
                return GetHolder(cast(pholder,ILogicalArrayHolder):value[piNum]).
            when type-of(pholder,IDateArrayHolder) then
                return GetHolder(cast(pholder,IDateArrayHolder):value[piNum]).
            when type-of(pholder,IDateTimeArrayHolder) then
                return GetHolder(cast(pholder,IDateTimeArrayHolder):value[piNum]).
            when type-of(pholder,IDateTimeTzArrayHolder)then
                return GetHolder(cast(pholder,IDateTimeTzArrayHolder):value[piNum]).
            when type-of(pHolder,ILongcharArrayHolder) then 
                return GetHolder(cast(pholder,ILongcharArrayHolder):value[piNum]).
            otherwise     
                undo, throw new NotImplementedError(subst("GetExtentValue for IPrimitiveArrayHolder of type &1",pholder:GetClass():TypeName)).
        end.
                  
    end method.
    
    method public static IPrimitiveHolder GetHolder(phFld as handle):
        define variable cLong as longchar no-undo.
        if phFld:available = false then
            undo, throw new IllegalOperationError(subst("GetHolder for field '&1' with no buffer available",phFld:name)). 
        
        // buffer-value resolution does not work for overload resolution so the buffe-value must be converted to variable    
        case phFld:data-type:
            when "character" then
                return GetHolder(string(phFld:buffer-value)).
            when "longchar" or when "clob" then
            do:
                cLong = phFld:buffer-value.
                return GetHolder(cLong).
            end.    
            when "int64" then
                return GetHolder(int64(phFld:buffer-value)).
            when "integer" then
                return GetHolder(integer(phFld:buffer-value)).
            when "decimal" then
                return GetHolder(decimal(phFld:buffer-value)).
            when "logical" then
                return GetHolder(logical(phFld:buffer-value)).
            when "date" then
                return GetHolder(date(phFld:buffer-value)).
            when "datetime" then
                return GetHolder(datetime(phFld:buffer-value)).
            when "datetime-tz" then
                return GetHolder(datetime-tz(phFld:buffer-value)).
            otherwise 
               undo, throw new UnsupportedOperationError(subst("GetHolder for buffer field '&1' of data type '&2'",phFld:name,phFld:data-type)). 
            
        end case.
        
    end.  
    
    method public static IPrimitiveHolder GetHolder(pValue as character):
        // we use a plain character to ensure the IPrimitiveHolder can be passed to 
        // the GetDataType in this class and used safely in reflection
        // OpendEge.Code.String is longchar 
        return new Pmfo.Core.Common.CharacterHolder(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as longchar):
        return new String(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as integer):
        // we use a plain integer holder to ensure the IPrimitiveHolder can be passed to 
        // the GetDataType in this class and then used safely in reflection
        // OpendEge.Code.Integer is int64 
        return new Pmfo.Core.Common.IntegerHolder(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as int64):
        return new OpenEdge.Core.Integer(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as decimal):
        return new OpenEdge.Core.Decimal(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as logical):
        return new OpenEdge.Core.LogicalValue(pValue).
    end method.
     
    method public static IPrimitiveHolder GetHolder(pValue as date):
        return new OpenEdge.Core.DateHolder(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as datetime):
        return new OpenEdge.Core.DateTimeHolder(pValue).
    end method. 
    
    method public static IPrimitiveHolder GetHolder(pValue as datetime-tz):
        return new OpenEdge.Core.DateTimeTzHolder(pValue).
    end method. 
    
    // arrays 
    method public static IPrimitiveArrayHolder GetArrayHolder(pValues as character extent):
        // we use Pmfo holder to ensure the IPrimitiveHolder can be passed to 
        // the GetDataType in this class and then used safely in reflection
        // OpendEge.Code.StringArrayHolder is longchar ! 
        return new Pmfo.Core.Common.CharacterArrayHolder(pValues).
    end method. 
    
    method public static IPrimitiveArrayHolder GetArrayHolder(pValues as longchar extent):
        // Openedsdge String is lonmgchar  
        return new OpenEdge.Core.StringArray(pValues).
    end method. 
    
    method public static IPrimitiveArrayHolder GetArrayHolder(pValues as int64 extent):
        // this is int 64! -  Pmfo has its own IntegerArrayHolder for plain integer
        return new Openedge.Core.IntegerArrayHolder(pValues).
    end method. 
    
    method public static IPrimitiveArrayHolder GetArrayHolder(pValues as integer extent):
        // we use the PMFO integer holder to ensure the IPrimitiveHolder can be passed to 
        // the GetDataType in this class and then used safely in reflection
        // OpendEge.Code.IntegerArrayHolder is int64 ! 
        return new Pmfo.Core.Common.IntegerArrayHolder(pValues).
    end method. 
    
    method public static void AddToJson(pName as character,
                                        pHolder   as IPrimitiveHolder,
                                        pJson     as JsonObject):
        case true   :
            when pHolder:GetClass():isA(get-class(ICharacterHolder)) then 
                pJson:Add(pName,cast(pholder,ICharacterHolder):value).

            when pHolder:GetClass():isA(get-class(IDateHolder)) then 
                pJson:Add(pName,cast(pholder,IDateHolder):value).

            when pHolder:GetClass():isA(get-class(IDateTimeHolder)) then 
                pJson:Add(pName,cast(pholder,IDateTimeHolder):value). 

            when pHolder:GetClass():isA(get-class(IDateTimeTzHolder)) then 
                pJson:Add(pName,cast(pholder,IDateTimeHolder):value).  

            when pHolder:GetClass():isA(get-class(IIntegerHolder)) then 
                pJson:Add(pName,cast(pholder,IIntegerHolder):value). 

            when pHolder:GetClass():isA(get-class(IInt64Holder)) then 
                pJson:Add(pName,cast(pholder,IInt64Holder):value). 
            
             when pHolder:GetClass():isA(get-class(IDecimalHolder)) then 
                pJson:Add(pName,cast(pholder,IDecimalHolder):value). 
                
            when pHolder:GetClass():isA(get-class(ILogicalHolder)) then
                pJson:Add(pName,cast(pholder,ILogicalHolder):value). 

            when pHolder:GetClass():isA(get-class(ILongcharHolder)) then
                pJson:Add(pName,cast(pholder,ILongcharHolder):value). 
            
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Primitive Function &1 Return type '&2'",pName,pHolder:GetClass():TypeName)).
        end.                                                 
    end method.              
    
    method public static void AssignToField(phField as handle,
                                            pHolder as IPrimitiveHolder):
        case true   :
            when pHolder:GetClass():isA(get-class(ICharacterHolder)) then 
                phField:buffer-value = cast(pholder,ICharacterHolder):value.

            when pHolder:GetClass():isA(get-class(IDateHolder)) then 
                phField:buffer-value =cast(pholder,IDateHolder):value.

            when pHolder:GetClass():isA(get-class(IDateTimeHolder)) then 
                phField:buffer-value =cast(pholder,IDateTimeHolder):value. 

            when pHolder:GetClass():isA(get-class(IDateTimeTzHolder)) then 
                phField:buffer-value =cast(pholder,IDateTimeHolder):value.  

            when pHolder:GetClass():isA(get-class(IIntegerHolder)) then 
                phField:buffer-value =cast(pholder,IIntegerHolder):value. 

            when pHolder:GetClass():isA(get-class(IInt64Holder)) then 
                phField:buffer-value =cast(pholder,IInt64Holder):value. 
            
             when pHolder:GetClass():isA(get-class(IDecimalHolder)) then 
                phField:buffer-value =cast(pholder,IDecimalHolder):value. 
                
            when pHolder:GetClass():isA(get-class(ILogicalHolder)) then
                phField:buffer-value =cast(pholder,ILogicalHolder):value. 

            when pHolder:GetClass():isA(get-class(ILongcharHolder)) then
                phField:buffer-value =cast(pholder,ILongcharHolder):value. 
            
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Field  &1 from  type '&2'",phField:name,pHolder:GetClass():TypeName)).
        end.                                                 
    end method.              
    
    
    method public static void AddToJson(pName as character,
                                        pHolder as IPrimitiveArrayHolder,
                                        pJson as JsonObject):
        define variable cVals as character extent no-undo.
        define variable iVals as integer extent no-undo.
        define variable i64vals as int64 extent no-undo.
        define variable decVals as decimal extent no-undo.
        define variable datVals as date extent no-undo.
        define variable dtiVals as datetime extent no-undo.
        define variable dtzVals as datetime-tz extent no-undo.
        define variable lvals as logical extent no-undo.
        define variable longvals as longchar extent no-undo.    
        define variable oJsonArray as JsonArray no-undo.     
                 
        oJsonArray = new JsonArray().     
        pJson:Add(pName,oJsonArray).  
        
        case true   :
            when pHolder:GetClass():isA(get-class(ICharacterArrayHolder)) then 
                oJsonArray:Add(cast(pholder,ICharacterArrayHolder):value).
                
            when pHolder:GetClass():isA(get-class(IDateArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IDateArrayHolder):value).
                
            when pHolder:GetClass():isA(get-class(IDateTimeArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IDateTimeArrayHolder):value).
                 
            when pHolder:GetClass():isA(get-class(IDateTimeTzArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IDateTimeArrayHolder):value).  
            
            when pHolder:GetClass():isA(get-class(IIntegerArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IIntegerArrayHolder):value).
                 
            when pHolder:GetClass():isA(get-class(IInt64ArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IInt64ArrayHolder):value).
            
            when pHolder:GetClass():isA(get-class(IDecimalArrayHolder)) then 
                oJsonArray:Add(cast(pholder,IDecimalArrayHolder):value).
                      
            when pHolder:GetClass():isA(get-class(ILogicalArrayHolder)) then
                oJsonArray:Add(cast(pholder,ILogicalArrayHolder):value).
                 
            when pHolder:GetClass():isA(get-class(ILongcharArrayHolder)) then
                oJsonArray:Add(cast(pholder,ILongcharArrayHolder):value). 
            
            otherwise 
               undo, throw new UnsupportedOperationError(subst("Primitive Function &1 Return type '&2'",pName,pHolder:GetClass():TypeName)).  
        end case.   
        catch e as Progress.Lang.Error :
            // just ignore cannot be indetermiate error as it is assumed to mean empty 
            if e:GetmessageNum(1) <> 16081 then
                undo, throw e.
        end catch.                                              
    end method.         
end class.
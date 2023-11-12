 
 /*------------------------------------------------------------------------
    File        : Method
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 08:41:17 EDT 2019
    Notes       : Subclasses should define the signatuees and parameters in 
                  CreateParameters so they are lazy loaded
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.Parser.FieldValueParser from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.ISignature from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Common.Signature from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.AssertUtil from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Util.DateUtil from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using OpenEdge.Core.DataTypeHelper from propath.
using Pmfo.Util.NumberUtil from propath.
using Progress.Json.ObjectModel.ObjectModelParser from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using OpenEdge.Core.DataTypeEnum from propath.

class Pmfo.Core.Common.Method abstract implements IMethod:

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    // what to call the type in error message 
    define public abstract property BaseTypeName  as character no-undo get.
    define public abstract property IsPrimitive   as logical   no-undo get. set.
    // only function as of current
   // define public abstract property Binding       as MethodBindingEnum no-undo get. set.
      
    define protected property IsCreated as logical no-undo get. set.
     
    // Tracked when findSignature is called succesfully to allow checks to get signature names
    // for example for cases that need to know the publicly exposed named parameters 
    // for cases where the data types are similar and different calls need to be handled 
    // by the same ABL method but we need to know which one    
    define public property CurrentParameterNames as character no-undo get. protected set.
       
    define protected property Signatures       as ISignature extent no-undo  
        get(pi as integer):
            if IsCreated = false then 
            do:
                CreateParameters().
                IsCreated = true.
            end.
            return Signatures[pi].
        end. 
        protected set.  
    
    define public property Name             as character         no-undo get. protected set. 
    define public property SingleObject     as logical           no-undo get. 
       set(plSingle as logical):
           AssertSingleObject(plSingle).
           SingleObject = plSingle.
       end method. 
    
    define public property ServerType       as ServerTypeEnum    no-undo 
         get():
             if not valid-object(ServerType) then
                 return ServerTypeEnum:General.
             else 
                 return ServerType.    
         end.
          
         // it is somewhat restrictive to make thos protected, but 
         // it cannot really be changed durong the lifetime so for now  
         // one will have to create dedicated class - It is only needed for 
         // actions that need a more server types than the main requested buffer 
         // so it is assumed to be rare.  
         // If necessary a consturctor can be added + BE AddAction and AddFucntion 
         protected set(pType as ServerTypeEnum):
             AssertUtil:AssertConstructorProperty().
             ServerType = pType.
         end. 
         
    define public property NoContentResponse as logical init ? no-undo 
        get():
            if NoContentResponse = ? then 
                return GetNoContentResponseDefault().
            else 
                return NoContentResponse.      
        end.     
        set. 
      
    constructor protected Method (pName as character ):
        super ().
        Name = pName.
    end constructor.
    // add serverttype constructors when needed 
    constructor protected Method (pName as character, pServerType as ServerTypeEnum ):
        super ().
        Name = pName.
        ServerType = pServerType.
    end constructor.
    
    constructor protected Method (pName as character,plObject as logical ):
        this-object(pName).
        SingleObject = plObject. 
    end constructor.
    
    constructor protected Method (pName as character, pParam as IParameter):
        this-object (pName,false).
        DefineSignature(pParam). 
    end constructor.
    
    constructor protected Method (pName as character, pParam as IParameter, pServerType as ServerTypeEnum):
        this-object (pName,false).
        DefineSignature(pParam). 
        ServerType = pServerType.
    end constructor.
    
    constructor protected Method (pName as character, pParams as IParameter extent ):
        this-object (pName,pParams,false).
    end constructor.
    
    constructor protected Method (pName as character, pParams as IParameter extent, pServerType as ServerTypeEnum ):
        this-object (pName,pParams,false).
        ServerType = pServerType.
    end constructor.
    
    constructor protected Method (pName as character, pParams as IParameter extent, plObject as logical ):
        this-object(pName,plObject).
        DefineSignature(pParams).
    end constructor.
    
    constructor protected Method (pName as character, pSignatures as ISignature extent):
        this-object (pName,pSignatures,false).
    end constructor.
    
    constructor protected Method (pName as character, pSignatures as ISignature extent, plObject as logical ):
        define variable i as integer no-undo.
        this-object(pName,plObject).
        //  add one by one so that AssertSignature will be called 
        do i = 1 to extent(pSignatures):
            AddSignature(pSignatures[i]). 
        end.
    end constructor.
    
    method protected abstract integer PrepareParameters(piNum as integer). 
    
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as character).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as longchar).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as integer).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as int64).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as decimal).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as logical).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as date).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as datetime).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as datetime-tz).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as character extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as longchar extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as integer extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as int64 extent) .
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as decimal extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as logical extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as date extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as datetime extent).
    method protected abstract void SetValue(pParameter as IParameter, piPosition as integer, pcValue as datetime-tz extent).
       
    method public ISignature DefineSignature(pParameters as IParameter extent):
       define variable oSignature as ISignature no-undo.
       oSignature = new Signature(pParameters).
       if AddSignature(oSignature) then
          return oSignature.
    end method.  
    
    // allow subclasses to control NoContentResponse default from other properties when not set 
    method protected logical GetNoContentResponseDefault ():
        return false. 
    end method.
    
    method public ISignature DefineSignature(pParameter as IParameter):
        define variable oSignature as ISignature no-undo.
        oSignature = new Signature(pParameter).
        if AddSignature(oSignature) then
            return oSignature.
        return ?.            
    end method.  
    
    //no parameters
    method public ISignature DefineSignature():
        define variable oSignature as ISignature no-undo.
        oSignature = new Signature().
        if AddSignature(oSignature) then
            return oSignature.
        return ?.            
    end method.  
    
    method protected void AssertSignature(pSignature as ISignature):
        define variable iSignature  as integer no-undo.
        define variable iParam      as integer no-undo.
        define variable iNewParam   as integer no-undo.
        
        if extent(Signatures)  = ? then
        do: 
            if extent(pSignature:Parameters) > 0 then
            do: 
                do iNewParam = 1 to extent(pSignature:Parameters):
                    if not valid-object(pSignature:Parameters[iNewParam]) then
                        undo, throw new IllegalArgumentError(subst("Signature[&1] Parameter &2 is undefined",extent(Signatures) + 1,inewParam)) .
                end.        
            end.
        end.    
        else do iSignature = 1 to extent(Signatures):
            if pSignature:Equals(Signatures[iSignature]) then
            do:
                if extent(pSignature:Parameters) = ? then 
                    undo, throw new IllegalArgumentError(subst("Signature[&1] already exists with no parameters",iSignature)) .
                else
                    undo, throw new IllegalArgumentError(subst("Signature[&1] already exists with the same parameters",iSignature)) .
            end.
            do iNewParam = 1 to extent(pSignature:Parameters):
                do iParam = 1 to extent(Signatures[iSignature]:Parameters):
                   
                   if not valid-object(pSignature:Parameters[iNewParam]) then
                       undo, throw new IllegalArgumentError(subst("Signature[&1] Parameter &2 is undefined",extent(Signatures) + 1,inewParam)) .
                        
                   if pSignature:Parameters[iNewParam]:name = Signatures[iSignature]:Parameters[iParam]:Name then
                   do: 
                       if pSignature:Parameters[iNewParam]:DataType <> Signatures[iSignature]:Parameters[iParam]:DataType then
                           undo, throw new IllegalArgumentError(subst("Cannot add parameter '&1' with data type '&2' since signature[&3] has parameter with the same name and data type '&4'",
                                                                      pSignature:Parameters[iNewParam]:name,
                                                                      pSignature:Parameters[iNewParam]:DataType,
                                                                      iSignature,
                                                                      Signatures[iSignature]:Parameters[iParam]:DataType
                                                                      )
                                                                ). 
                           
                           
                   end.          
                end.        
            end.
        end.
        
    end method.     
    
    // subclasses may need to assert this based on other properties 
    method protected void AssertSingleObject(pSingle as logical):
    end method.    
    
    method protected logical AddSignature(pSignature as ISignature):
        define variable oSignatures as ISignature extent no-undo.
        define variable i as integer no-undo.
        
        AssertSignature(pSignature).
        
        if extent(Signatures) > 0 then
        do:
            oSignatures  = Signatures.
            extent(Signatures) = ?.
            extent(Signatures) = extent(oSignatures) + 1.
            do i = 1 to extent(oSignatures):
                Signatures[i] = oSignatures[i].
            end.
            Signatures[i] = pSignature.
            
        end.
        else do:
            extent(Signatures) = 1.
            Signatures[1] = pSignature.
        end.    
        
        return true.
    end method.
    
    method protected void ParseExpression(pExpression as character):
        define variable oParameters as IStringStringMap no-undo.
        define variable oFieldValueParser as FieldValueParser no-undo.
        if pExpression gt "" and index(pExpression,"=") = 0 then 
            undo, throw new IllegalArgumentError(subst("The  &1 reference '/&2(&3)' in the URL is invalid. Requests for &1s must have named parameters or no parameters.",
                                                        BaseTypeName,
                                                        Name,
                                                        pExpression
                                                        )
                                                ).
         
        oFieldValueParser = new FieldValueParser().
        oParameters = oFieldValueParser:ParseExpressions(pExpression,true). // true keep quotes
        SetParameters(oParameters, true).
    end method.
    
    // subclasses that want to add stuff override to track this 
    // TODO - this is a hack  add poper support 
    method protected void SetCurrentParameters(pParameters as IStringStringMap): 
    end method.
    
    method protected character ParseExpression(pExpression as character,poAliases as IStringStringMap):
        define variable oParameters        as IStringStringMap no-undo.
        define variable oFieldValueParser  as FieldValueParser no-undo.
        define variable cParameterNames    as character        no-undo. 
        define variable oSignature         as ISignature       no-undo.
        define variable i                  as integer no-undo.
        define variable cValue             as character no-undo.
        if pExpression gt "" and index(pExpression,"=") = 0 then 
            undo, throw new BadRequestError(subst("The  &1 reference '/&2(&3)' in the URL is invalid. Requests for &1s must have named parameters or no parameters.",
                                                        BaseTypeName,
                                                        Name,
                                                        pExpression
                                                        )
                                                ).
         
        oFieldValueParser = new FieldValueParser().
        
        oParameters = oFieldValueParser:ParseExpressions(pExpression,true,poAliases). // true keep quotes
        SetCurrentParameters(oParameters).
        if valid-object(poAliases) then
        do: 
            // if aliases are sued then add singl;e quites if applicable 
            // TODO  this needs cleanup -  
            // - pass data type to FieldValueparser - probably diffcilt since we we find singatrue basad on parameters parsed 
            // - or move find signature higher up oin call stack 
            //   setParameters is supporting alernatives that seems unnecessary - no quites - array of values (did not improve performance
             //     check MachineTalk API where parameters are created on the fly         
            if oFieldValueParser:ParametersWithAliases > "" then
            do on error undo, throw: 
                cParameterNames = GetParameterNames(oParameters).
                oSignature = FindSignature(cParameterNames).
                do i = 1 to extent(oSignature:Parameters): 
                    if (oSignature:Parameters[i]:DataType = DataTypeEnum:Character 
                        or oSignature:Parameters[i]:DataType = DataTypeEnum:Date
                        or oSignature:Parameters[i]:DataType = DataTypeEnum:DateTime
                        or oSignature:Parameters[i]:DataType = DataTypeEnum:Datetime-TZ
                        or oSignature:Parameters[i]:DataType = DataTypeEnum:LongChar)
                    and lookup(oSignature:Parameters[i]:name,oFieldValueParser:ParametersWithAliases) > 0 then
                    do:
                        cValue = oParameters:Get(oSignature:Parameters[i]:name).
                        oParameters:Put(oSignature:Parameters[i]:name,quoter(cValue,"'")).  
                    end.        
                end.
                catch e as Progress.Lang.Error :
                    // this is hack - any errors here handle in set paramters    
                end catch.
            end.     
        end.
        
        SetParameters(oParameters, true).
        // hack
        return oFieldValueParser:ReplacedAliases.
        catch e as Progress.Lang.Error :
            if pExpression > "" then
                undo, throw new BadRequestError(subst("&1 /&2(&3) in URL has invalid parameter(s) or value(s)",
                                                       BaseTypeName,
                                                       Name,
                                                       pExpression 
                                                     ),
                                                e).
            else
                undo, throw e.                                   
                                                
        end catch.
        
    end method.
    
    method protected character GetParameterNames(pParameterMap as IStringStringMap): 
        define variable oIterator       as IIterator  no-undo.
        define variable cParameterNames as character no-undo.
        define variable cDLm            as character no-undo. 
        if valid-object(pParameterMap) then 
        do:
            if pParameterMap:Size > 0 then
            do:
                oIterator = pParameterMap:KeySet:Iterator().
                do while oIterator:HasNext():
                    assign
                        cParameterNames = cParameterNames 
                                        + cDlm 
                                        + string(cast(oIterator:Next(),String):Value)
                        cDlm            = ","
                        .
                end.
              
            end.
        end.
        return cParameterNames.
    end method.       
    
    // final to manage ISCreated - overrides should typically be done in SetValue or ParseExpression
    method final protected void SetParameterExpression(pExpression as character).
        
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end.
        
        ParseExpression(pExpression). 
        
    end method. 
    
     // final to manage ISCreated - overrides should typically be done in SetValue or ParseExpression
    method final protected character SetParameterExpression(pExpression as character, pcAliases as IStringStringMap).
        
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end.
        
        return ParseExpression(pExpression,pcAliases). 
        
    end method. 
    
    method protected final void SetParameters(pParameters as IStringStringMap, plIsQuoted as logical):
        define variable cDummy as character extent no-undo.
        SetParameters(pParameters, cDummy, cDummy, plIsQuoted ). 
    end method.
    
    method protected final void SetParameters(pParameters as character extent, pValues as character extent, plIsQuoted as logical):
        SetParameters(?, pParameters, pValues, plIsQuoted ). 
    end method.
    
    method private final void SetArrayParameter(pParameter as IParameter, pPosition as integer,pcValue as longchar):
        define variable oJsonParser as ObjectModelParser no-undo.
        define variable oJsonArray  as JsonArray no-undo.
        define variable i as integer no-undo.
        
        define variable cvals   as character   extent no-undo. 
        define variable cLongs  as longchar    extent no-undo. 
        define variable iInts   as integer     extent no-undo.
        define variable iInt64s as int64       extent no-undo.
        define variable dVals   as decimal     extent no-undo.
        define variable lVals   as logical     extent no-undo.
        define variable dDates  as date        extent no-undo.
        define variable dDateTimes as datetime extent no-undo.
        define variable dDateTimeTzs as datetime-tz extent no-undo.
        define variable cBadArrayTemplate as character no-undo
            init "Parameter '&1' value &2 is not a valid array". 
        oJsonParser = new ObjectModelParser().
        pcValue = codepage-convert (pcValue,"utf-8").        
        do on error undo, throw:
            // The parser does not detect bad character after the right bracket 
            if (string(pcValue) matches "[*]") = false then
                undo, throw new BadRequestError(subst(cBadArrayTemplate,pParameter:name,pcValue)).      
            
            oJsonArray = cast(oJsonParser:Parse(pcValue),JsonArray).
             
            catch e1 as BadRequestError :
                undo, throw e1.    
            end catch.
            catch e2 as Progress.Lang.Error :
                undo, throw new BadRequestError(subst(cBadArrayTemplate,pParameter:name,pcValue),e2).      
            end catch. 
        end.
        if oJsonArray:length > 0 then 
        do: 
            case pParameter:DataType:
                when DataTypeEnum:LongCharArray then 
                    extent(cLongs) = oJsonArray:length.
                when DataTypeEnum:CharacterArray then 
                    extent(cVals) = oJsonArray:length.
                when DataTypeEnum:LogicalArray then 
                    extent(lVals) = oJsonArray:length.
                when DataTypeEnum:IntegerArray then 
                    extent(iInts) = oJsonArray:length.
                when DataTypeEnum:Int64Array then 
                    extent(iInt64s) = oJsonArray:length.
                when DataTypeEnum:DecimalArray  then 
                    extent(dVals) = oJsonArray:length.
                when DataTypeEnum:DateArray then 
                    extent(dDates) = oJsonArray:length.
                when DataTypeEnum:DatetimeArray then 
                    extent(dDateTimes) = oJsonArray:length.
                when DataTypeEnum:DatetimeTZArray then 
                    extent(dDateTimeTzs) = oJsonArray:length.
                otherwise 
                    undo, throw new IllegalStateError(subst("Parameter &1 DataType '&2' is unsupported or invalid",pParameter:name,pParameter:DataType)).       
              
            end.
            do i = 1 to oJsonArray:length
            on error undo, throw:
                case oJsonArray:GetType(i):
                    when JsonDataType:NUMBER then
                    do:
                        case pParameter:DataType:
                            when DataTypeEnum:IntegerArray then
                                iInts[i] = oJsonArray:GetInteger(i).   
                            when DataTypeEnum:Int64Array then 
                                iInt64s[i] = oJsonArray:GetInt64(i).   
                            when DataTypeEnum:DecimalArray  then
                                dVals[i] = oJsonArray:GetDecimal(i).
                            otherwise do:
                                ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                            end.       
                        end case.
                    end.
                    when JsonDataType:BOOLEAN then
                    do:
                        case pParameter:DataType:
                            when DataTypeEnum:LogicalArray then
                                lVals[i] = oJsonArray:GetLogical(i).   
                            otherwise do:
                                ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                            end.       
                        end case.
                    end.
                    when JsonDataType:STRING then
                    do: 
                        case pParameter:DataType:
                            when DataTypeEnum:CharacterArray then
                                cVals[i] = oJsonArray:GetCharacter(i).   
                            when DataTypeEnum:LongCharArray then 
                                cLongs[i] = oJsonArray:GetLongchar(i).   
                            when DataTypeEnum:DateArray  then
                                dDates[i] = oJsonArray:GetDate(i).
                            when DataTypeEnum:DatetimeArray  then
                                dDateTimes[i] = oJsonArray:GetDatetime(i).
                            when DataTypeEnum:DatetimeTZArray  then
                                dDateTimeTzs[i] = oJsonArray:GetDatetimeTZ(i).
                            otherwise do:
                                ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                            end.       
                        end.
                    end.     
                    when JsonDataType:NULL then
                    do: 
                        case pParameter:DataType:
                            when DataTypeEnum:IntegerArray then
                                iInts[i] = ?.   
                            when DataTypeEnum:Int64Array then 
                                iInt64s[i] = ?.   
                            when DataTypeEnum:DecimalArray  then
                                dVals[i] = ?.
                            when DataTypeEnum:LogicalArray then
                                lVals[i] = ?.
                            when DataTypeEnum:CharacterArray then
                                cVals[i] = ?.   
                           // these are not really needed since ? is defaailt for longchar and dates      
                            when DataTypeEnum:LongCharArray then 
                                cLongs[i] = ?.   
                            when DataTypeEnum:DateArray  then
                                dDates[i] = ?.
                            when DataTypeEnum:DatetimeArray  then
                                dDateTimes[i] = ?.
                            when DataTypeEnum:DatetimeTZArray  then
                                dDateTimeTzs[i] = ?.       
                       end.         
                    end.
                    otherwise
                        undo, throw new BadRequestError(subst("Parameter &1 value is not a primitive value",pParameter:name)).   
      
                end case.
                catch e as Progress.Lang.Error :
                    undo, throw new BadRequestError(subst("Attempting to set &1 parameter &2[3] to &4",
                                                           // entry 1 to not show extent in text if present (we show the number in bracket)
                                                           entry(1,DataTypeHelper:GetMask(pParameter:DataType)," "),
                                                           pParameter:name,
                                                           i,
                                                           JsonUtil:GetStringValue(oJsonArray, i))
                                                           ,e).    
                                                                   
                end catch. 
            end.
        end.
        case pParameter:DataType:
            when DataTypeEnum:IntegerArray then
                SetValue(pParameter, pPosition, iInts).   
            when DataTypeEnum:Int64Array then 
                SetValue(pParameter, pPosition, iInt64s).   
            when DataTypeEnum:DecimalArray  then
                SetValue(pParameter, pPosition, dVals).
            when DataTypeEnum:LogicalArray then
                SetValue(pParameter, pPosition, lVals).
            when DataTypeEnum:CharacterArray then
                SetValue(pParameter, pPosition, cVals).   
           // these are not really needed since ? is defaailt for longchar and dates      
            when DataTypeEnum:LongCharArray then 
                SetValue(pParameter, pPosition, cLongs).   
            when DataTypeEnum:DateArray  then
                SetValue(pParameter, pPosition, dDates).
            when DataTypeEnum:DatetimeArray  then
                SetValue(pParameter, pPosition, dDateTimes).
            when DataTypeEnum:DatetimeTZArray  then
                SetValue(pParameter, pPosition, dDateTimeTzs).     
        end case.
        
    end method.     
    
    // we suypport to alernative ways to provide data  -  
    method private final void SetParameters(pParameterMap as IStringStringMap, pParameters as character extent, pValues as character extent, plIsQuoted as logical):
        define variable oSignature      as ISignature no-undo.
        define variable cDlm            as character  no-undo.
        define variable cParameterNames as character  no-undo.
        define variable i               as integer    no-undo.
        define variable cName           as character  no-undo.
        define variable cDataType       as character  no-undo.
        define variable cValue          as longchar   no-undo.
        define variable cOriginalValue  as longchar   no-undo.
        define variable cStringType     as character  no-undo.
        define variable iInteger        as integer    no-undo.
        define variable iInt64          as int64      no-undo.
        define variable cQuote          as character no-undo.
        define variable iStart          as integer no-undo.
        
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end. 
        if valid-object(pParameterMap) and extent(pValues) > 0 then 
             undo, throw new IllegalArgumentError("The SetParameters must either use IStringStringMap or value and parameter extents").
        
        if valid-object(pParameterMap) then 
        do:
            cParameterNames = GetParameterNames(pParameterMap). 
        end.
        else            
        do: 
            do i = 1 to extent(pParameters):
                assign
                    cParameterNames = cParameterNames 
                                    + cDlm 
                                    + pParameters[i] 
                cDlm                = ","       
                .
            end.
        end.
        oSignature = FindSignature(cParameterNames).
        // in some cases the PostSetValues at the end here is not sufficient 
        // if for example server side specific valu needs to be added
        // in that case the CurrentParamterNames can be used to check which are set 
        CurrentParameterNames = cParameterNames. // public - see definition 
        
        iStart = PrepareParameters(extent(oSignature:Parameters)).
        
        do i = iStart to extent(oSignature:Parameters) on error undo, throw:
            
            cValue = if valid-object(pParameterMap) 
                     then pParameterMap:Get(oSignature:Parameters[i]:name) 
                     else pValues[i].
                 
            if cValue begins "[" then 
            do:
                if oSignature:Parameters[i]:IsArray = false then 
                    undo, throw new BadRequestError(subst("Parameter '&1' is not an array and the value should not be in brackets",oSignature:Parameters[i]:name)). 
                if valid-object(pParameterMap) = false or plIsQuoted = false then 
                    undo, throw new IllegalStateError("SetParameters that has values in brackets is called with illegal/invalid parameter(s)"). 
                
                SetArrayParameter(oSignature:Parameters[i], i, cValue).    
            end.
            else do:  
                if oSignature:Parameters[i]:IsArray then 
                    undo, throw new BadRequestError(subst("Parameter '&1' is an  array and the value must be in brackets",oSignature:Parameters[i]:name)). 
                            
                cStringType = DataTypeHelper:GetMask(oSignature:Parameters[i]:DataType) .
                cOriginalValue = cValue. // for error messages          
                // unknown values may be quoted - 
                if cValue = "'?'" or cValue = '"?"' then 
                    cValue = ?.
                else if plIsQuoted then
                do: 
                   cValue = StripAndAssertQuotes(cValue,oSignature:Parameters[i]).
                end.
               
                do on error undo, throw:
                    case oSignature:Parameters[i]:DataType:
                        when DataTypeEnum:Character then
                            SetValue(oSignature:Parameters[i],i, string(cValue)).
                        when DataTypeEnum:LongChar then
                            SetValue(oSignature:Parameters[i],i, cValue).
                        when DataTypeEnum:Integer then
                        do: 
                            do on error undo, throw:
                                iInteger = integer(cValue).
                                catch e as Progress.Lang.Error:
                                    ThrowDataTypeError(oSignature:Parameters[i], cValue).
                                end catch.
                            end.     
                            if NumberUtil:NumDecimals(decimal(cValue)) > 0 then 
                                ThrowDataTypeError(oSignature:Parameters[i], cValue).
                            SetValue(oSignature:Parameters[i],i, iInteger).
                        end.
                        when DataTypeEnum:Int64 then
                        do:
                            do on error undo, throw:
                                iInt64 = int64(cValue).
                                catch e as Progress.Lang.Error:
                                    ThrowDataTypeError(oSignature:Parameters[i], cValue).
                                end catch.
                            end.
                            if NumberUtil:NumDecimals(decimal(cValue)) > 0 then 
                                ThrowDataTypeError(oSignature:Parameters[i], cValue).
                              
                            SetValue(oSignature:Parameters[i],i, iint64).
                               
                        end.
                        when DataTypeEnum:Decimal then
                            SetValue(oSignature:Parameters[i],i, dec(cValue)).
                        when DataTypeEnum:Logical then
                            SetValue(oSignature:Parameters[i],i, logical(cValue)).
                        when DataTypeEnum:Date then 
                        do:
                            SetValue(oSignature:Parameters[i],i, DateUtil:DateFromIsoDate(string(cValue))).
                        end.
                        when DataTypeEnum:Datetime then
                            SetValue(oSignature:Parameters[i],i, datetime(cValue)).
                        when DataTypeEnum:DatetimeTZ then
                            SetValue(oSignature:Parameters[i],i, datetime-tz(cValue)).
                        otherwise 
                            undo, throw new IllegalStateError(subst("Parameter &1 DataType '&2' is unsupported or invalid",oSignature:Parameters[i]:name,oSignature:Parameters[i]:DataType)).       
                    end.
                    catch e1 as BadRequestError:
                        undo, throw e1.
                    end .     
                    catch e2 as Progress.Lang.Error :
                        undo, throw new IllegalArgumentError(subst("Attempting to set &1 parameter &2 to &3",
                                                                    cStringType,
                                                                    oSignature:Parameters[i]:name,
                                                                    string(cOriginalValue))
                                                               ,e2).    
                                                               
                    end catch.
                end.
            end. // not an array
        end.  
        PostSetValues(cParameterNames).
    end method.
    
    method protected longchar StripAndAssertQuotes(pcValue as longchar, pParam as IParameter) :
        define variable cQuote as character no-undo.
        define variable cValue as character no-undo.
        define variable cStringType as character no-undo.
        cStringType = DataTypeHelper:GetMask(pParam:DataType) .
         
        cValue = StringUtil:UnquoteValue(pcValue,output cQuote).
        if cQuote > "" then               
        do:
            if lookup(cStringType,"integer,int64,decimal") > 0 then 
                undo, throw new BadRequestError(subst("Parameter '&1' is a number and the value should not be quoted",pParam:name)). 
            else if cStringType = "logical" then 
                undo, throw new BadRequestError(subst("Parameter '&1' is boolean and the value should not be quoted",pParam:name)). 
            
            if cQuote = '"' then 
                undo, throw new BadRequestError(subst("Parameter '&1' value is  ouble quoted. &2 string values must be single quoted.",pParam,BaseTypeName)). 
                       
        end.
        else do:
           if cValue <> ? then
           do: 
               if lookup(cStringType,"character,longchar") > 0 then 
                   undo, throw new BadRequestError(subst("Parameter '&1' is a string and should be quoted",pParam:name)). 
               else if lookup(cStringType,"date,datetime,datetime-tz") > 0 then 
                   undo, throw new BadRequestError(subst("Parameter '&1' is a Date and should be quoted",pParam:name)).
           end.     
        end. 
        return cValue.
    end method.    
     
    method protected void ThrowDataTypeError(pParam as IParameter,  pcValue as longchar):
        undo, throw new BadRequestError(subst("Invalid value &1 for parameter '&2'. The value is not a valid ABL '&3' data-type value.",
                                              pcValue, 
                                              pParam:name,
                                              // entry 1 to not show extent if present
                                              entry(1,DataTypeHelper:GetMask(pParam:DataType)," ")
                                             )
                                       ).
    end method.
    
    // allow subclasses override for conditional logic based on signature in use
    method protected void PostSetValues(parameternames as character):
    end method.    
    
    method protected ISignature FindSignature(pcParameterNames as character):
        define variable iSignature as integer no-undo.
        define variable iParameter as integer no-undo.
        define variable iMatches   as integer no-undo.
        define variable oParameter as IParameter no-undo.
        
        Signatureloop:
        do iSignature = 1 to extent(Signatures):
            iMatches = 0.                 
            if pcParameterNames = "" and extent(Signatures[iSignature]:Parameters) = ? then 
                return Signatures[iSignature].
            do iParameter = 1 to extent(Signatures[iSignature]:Parameters).
               oParameter = Signatures[iSignature]:Parameters[iParameter].
               if not valid-object(oParameter) then  
                   undo, throw new IllegalStateError(subst("&1 '&2' Parameter[&3] is undefined",BaseTypeName,Name,iParameter)). 
               if lookup(oParameter:name,pcParameterNames) > 0 then
                  iMatches = iMatches + 1.
               else do:
                  next signatureloop.
               end.      
            end.
            if iMatches = num-entries(pcParameterNames) then 
                return Signatures[iSignature].    
        end.  
        if pcParameterNames = "" then 
            undo, throw new BadRequestError(subst("&1 '&2' has no Signature without parameters",BaseTypeName,name)).
        else    
            undo, throw new BadRequestError(subst("&1 '&2' has no Signature with parameters &3",
                                                  BaseTypeName,
                                                  name,
                                                  StringUtil:GetMessageFromEntries(pcParameterNames)
                                                  )).  
    end method.
    
    method public final character GetPublicLinkTemplate(piSignature as integer):
        
        define variable iFld             as integer   no-undo.
        define variable cExpression      as character  no-undo.
        
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end.
        
        if extent(Signatures) = ? then  
           return Name. // This means an empty signature.  
   
        if extent(Signatures) > 0 and piSignature > 0 and extent(Signatures) ge piSignature then
        do:
            if extent(Signatures[piSignature]:Parameters) = ? then
               return Name.

            do iFld = 1 to extent(Signatures[piSignature]:Parameters):
                cExpression = cExpression 
                            + (if cExpression = "" then "" else ",")
                            + subst("&1=@p&2",Signatures[piSignature]:Parameters[iFld]:name,iFld).
            
            end.
            return subst("&1(&2)",Name,cExpression).
        end.    
        
        else undo, throw new IllegalArgumentError(subst("&1 is out of range. There are &2 Signatures",piSignature,extent(Signatures))). 
              
    end method. 
    
    // final - manages IsCreated
    method public final logical IsParametersRequired():
        define variable i as integer no-undo.
        
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end.
        
        if extent(Signatures) = ? then 
           return false. // This means an empty signature. The lazy load will kick in when a Signature array is requested
        
        do i = 1 to extent(Signatures):
            if extent(Signatures[i]:Parameters) = ? then
                return false.
        end.
        return true. // all signatures have parameters if we get here.   
    end method.
    
    // FOr Catalog - final - manages IsCreated
    method public final JsonArray DescribeSignatures(pcPath as character, phBuffer as handle):
        define variable jsonSigArray as JsonArray no-undo.
        define variable jsonParamValues as JsonArray no-undo.
        define variable jsonSig as JsonObject no-undo.
        define variable jsonProp as JsonObject no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
       
        if IsCreated = false then 
        do:
            IsCreated = true.
            CreateParameters().
        end.
        
        if extent(Signatures) <> ? then
        do:
            jsonSigArray = new JsonArray().
            do i = 1 to extent(Signatures):
               jsonSig = new JsonObject().
               jsonSigArray:Add(jsonSig).     
               
               do j = 1 to extent(Signatures[i]:Parameters):
                  jsonProp = new JsonObject().
                  jsonSig:Add(Signatures[i]:Parameters[j]:Name,jsonProp).
                  jsonProp:Add("datatype",JsonUtil:GetJsonType(Signatures[i]:Parameters[j]:DataType)).
                  jsonParamValues = GetValidParameterValues( Signatures[i]:Parameters[j]:Name).
                  if valid-object(jsonParamValues) and jsonParamValues:Length > 0 then 
                      jsonProp:Add("validValues",jsonParamValues). 
               end.
               jsonSig:Add("pathTemplate",DescribeSignaturePath(pcPath,i,phbuffer)).
           end.
       end.
       return jsonSigArray.
            
        
    end method.    
    
    // override to add valid values to parameter = TODO replace with enum support 
    method protected JsonArray GetValidParameterValues(pcName as character):
        return ?.
    end method.    
    
    
    method protected character DescribeSignaturePath(pcPath as character, piSignature as integer,phBuffer as handle):
        return subst("&1/&2",pcPath,GetPublicLinkTemplate(piSignature)).
    end method.    
    
    //  lazy load - sub classes override this to create parameters   
    method protected void CreateParameters(): 
    end method.      
    
    method public override  character ToString():
        define variable i as integer no-undo.
        define variable cString as character no-undo.
        cString = name.
        do i = 1 to extent(Signatures):
            cString = StringUtil:AppendEntry(cString, Signatures[i]:ToString()," ").           
        end.       
        return cString.
    end method.
end class.
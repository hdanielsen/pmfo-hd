 
 /*------------------------------------------------------------------------
    File        : TestFindMethod
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat May 01 07:31:43 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.BusinessLogic.Parser.FieldValueParser from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Flags from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Parameter from propath.
using Progress.Json.ObjectModel.ObjectModelParser from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Pmfo.Util.JsonUtil from propath.

class Pmfo.Core.BusinessLogic.Parser.MethodParser:
    
    define temp-table ttOutput no-undo
        field position        as integer
        field dataType        as character
        field characterValue  as character
        field integerValue    as integer
        field decimalValue    as decimal
        field int64Value      as int64
        field logicalValue    as logical
        field dateValue       as date
        field dateTimeValue   as datetime
        field dateTimeTzValue as datetime-tz
        field clobValue       as clob
        .
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public MethodParser (  ):
        super ().
    end constructor.
     
    method public JsonObject Invoke(pEntity as IServiceEntity,pRequest as IClientRequest, pResponNodeName as character).
        define variable oParameters       as IStringStringMap no-undo.
        define variable oFieldValueParser as FieldValueParser no-undo.
        define variable cMethodName       as character no-undo.
        define variable oMethod           as Method no-undo. 
        define variable oParameterList    as ParameterList no-undo.
        define variable oMethodParameters as Parameter extent no-undo.
        define variable oJsonResponse     as JsonObject no-undo.
        define variable oJsonDetails      as JsonObject no-undo.
        
        oFieldValueParser = new FieldValueParser().
        if pRequest:HasAlias then
        do:
            oParameters = oFieldValueParser:ParseExpressions(pRequest:KeyWheres[extent(pRequest:KeyWheres)], false,pRequest:Aliases).
            pRequest:AddUsedAliases(oFieldValueParser:ReplacedAliases).
        end.
        
        else
            oParameters = oFieldValueParser:ParseExpressions(pRequest:KeyWheres[extent(pRequest:KeyWheres)]).
        if extent(pRequest:Collections) > 1 then
        do: 
            cMethodName = pRequest:Collections[extent(pRequest:Collections)]. 
        end.
        else if pEntity:MethodOnly then 
            undo, throw new IllegalOperationError(subst("Requests to Service '&1' must specify an operation (action or function) in the URL after the Service name",pEntity:Resource)).
             
        oMethod = GetMethod(pEntity, cMethodName, oParameters).
        
        if not valid-object(oMethod) then 
        do: 
            if cMethodName > "" then 
                undo, throw new NotFoundError(pRequest:ServiceURI).
            // if no name then assume object that only returns properties 
            return ?.     
        end.
         
        if cMethodName > "" // don't check for execute method
        and pEntity:MethodOperationType(oMethod:Name) <> pRequest:Method then 
        do: 
            undo, throw new MethodNotAllowedError(pRequest:Method,subst("&1/&2",pEntity:Resource,oMethod:Name)).
        end.      
        
        oMethodParameters = oMethod:GetParameters().
        
        oParameterList = new ParameterList(oMethod:NumParameters).
        
        if oParameterList:NumParameters > 0 then
        
            PrepareInputParameters(pEntity,oMethod,oParameterList, oParameters).
        
        if Progress.Reflect.DataType:Void:Equals(oMethod:ReturnType) then 
        do:
           if oParameterList:NumParameters > oParameters:Size then
               PrepareOutputParameters(oMethodParameters,oParameterList). 
           
           oMethod:Invoke(pEntity,oParameterList).  
              
           if oParameterList:NumParameters > oParameters:Size then
           do: 
               oJsonResponse = new JsonObject().
               oJsonDetails = new JsonObject().
               AddOutputValuesToJson(pEntity,oMethod,oJsonDetails).
               oJsonResponse:Add(pResponNodeName,oJsonDetails).
           end.   
        end.
        else do: 
            if oMethod:ReturnTypeName = "Progress.Json.ObjectModel.JsonObject" then
                 oJsonREsponse = oMethod:Invoke(pEntity,oParameterList).
            else do:
                oJsonResponse = new JsonObject().
                if oMethod:ReturnExtent = 0 then
                    InvokeAndAddValueToJson(pEntity, oMethod, oParameterList,oJsonResponse,pResponNodeName).           
                else 
                    InvokeAndAddExtentValuesToJson(pEntity, oMethod, oParameterList,oJsonResponse,pResponNodeName).
            end.       
        end. 
        if pRequest:HasAlias then 
            pRequest:AssertUriQueryAliases().  
        return oJsonResponse. 
    end method. 
    
    // Get the requested method that matches the paramters 
    // May return ? for methodname "" as the entoty may only have properties and no method 
    method protected Method GetMethod(pEntity as IServiceEntity,pcMethodName as character,pParameters as IStringStringMap).
        define variable oValues as String extent no-undo.
        define variable cParameterNames as character no-undo.
        define variable i as integer no-undo.
        if pParameters:Size > 0 then
        do:
            oValues = cast(pParameters:KeySet:ToArray(),String).
            cParameterNames = Array:ToString(oValues).
        end.
        if pcMethodName = "" then
        do: 
            return GetDefaultMethod(pEntity, cParameterNames).
        end.
        else    
            return FindSignature(pEntity, pcMethodName, cParameterNames).
        
    end method.
    
    method protected Flags GetFlags():
        return Progress.Reflect.Flags:Public or Progress.Reflect.Flags:DeclaredOnly or Progress.Reflect.Flags:Instance. 
    end method.
    
   
    method protected Method FindSignature(pEntity as IServiceEntity,pcMethodName as character,pcParameterNames as character):
        define variable IMethod as integer no-undo.
        define variable iParameter as integer no-undo.
        define variable iMatches   as integer no-undo.
        define variable oMethods   as Method extent no-undo.
        define variable oParameters as Parameter extent no-undo.
        
        oMethods = pEntity:GetMethods().
        Methodloop:
        do IMethod = 1 to extent(oMethods):
            if oMethods[iMethod]:name = pcMethodName then
            do:
                iMatches = 0.                
                extent(oParameters) = ?. 
                oParameters = oMethods[IMethod]:GetParameters().
                
                if pcParameterNames = "" and extent(oParameters) = ? then 
                     
                    return oMethods[IMethod].
                
                do iParameter = 1 to extent(oParameters).  
                   if oParameters[iParameter]:Mode = Progress.Reflect.ParameterMode:Input 
                   and lookup(pEntity:GetPublicParameterName(oParameters[iParameter]:name),pcParameterNames) > 0 then
                       iMatches = iMatches + 1.
                   
                   else do:
                       next MethodLoop.
                   end.      
                end.
                if iMatches = num-entries(pcParameterNames) then 
                    return oMethods[IMethod].
            end.        
        end.  
        if pcParameterNames = "" then 
            undo, throw new NotFoundError(subst("&1 '&2' without parameters",pEntity:Resource,pcMethodName)).
        else    
            undo, throw new NotFoundError(subst("&1 '&2' with parameters &3",
                                                  pEntity:Resource,
                                                  pcMethodName,
                                                  StringUtil:GetMessageFromEntries(pcParameterNames)
                                                )
                                         ).  
    end method.
    
      // Get the single default method for the entity - return ? for methodname "" and no parameters since
      // the entity mau have have properties and no method 
    method protected Method GetDefaultMethod(pEntity as IServiceEntity,pcParameterNames as character):
        define variable IMethod as integer no-undo.
        define variable iParameter as integer no-undo.
        define variable iMatches   as integer no-undo.
        define variable oMethods   as Method extent no-undo.
        define variable oParameters as Parameter extent no-undo.
        
        // this is allowed wuth no parameters
        if not valid-object(pEntity:Method) then
        do: 
            if pcParameterNames = "" then 
                return ?. 
                
            undo, throw new NotFoundError(subst("&1 with parameters",pEntity:Resource)).
        end.
              
        oParameters = pEntity:Method:GetParameters().
         
        if pcParameterNames = "" and extent(oParameters) = ? then 
            return pEntity:Method.
                
        do iParameter = 1 to extent(oParameters).
            if oParameters[iParameter]:Mode = Progress.Reflect.ParameterMode:Input 
            and lookup(pEntity:GetPublicParameterName(oParameters[iParameter]:name),pcParameterNames) > 0 then
                iMatches = iMatches + 1.
            else 
                leave. 
        end.
        
        if iMatches > 0 and iMatches = num-entries(pcParameterNames) then
            return pEntity:Method.
            
        if pcParameterNames = "" then 
            undo, throw new NotFoundError(subst("&1 without parameters",pEntity:Resource)).
        else    
            undo, throw new NotFoundError(subst("&1 with parameters &2",
                                                pEntity:Resource,
                                                StringUtil:GetMessageFromEntries(pcParameterNames)
                                                )
                                         ).  
                                                  
    end method.
     
    method protected void PrepareInputParameters(pEntity as IServiceEntity,pMethod as Method,pParameterList as ParameterList,pParameters as IStringStringMap):
        define variable iPosition           as integer   no-undo.
        
        define variable cInternalInputNames as character no-undo.
        define variable cName               as character no-undo.
        define variable cValue              as longchar no-undo.
        define variable oMethodParameters   as Parameter extent no-undo.  
        define variable oIterator as IIterator  no-undo.
        define variable oMapEntry as IMapEntry  no-undo.
        define variable cDataType as character no-undo.        
        oIterator = pParameters:EntrySet:Iterator().  
        oMethodParameters = pMethod:GetParameters().
        do iPosition = 1 to extent(oMethodParameters):
            cInternalInputNames = cInternalInputNames 
                                + (if iPosition = 1 then "" else ",") 
                                + if oMethodParameters[iPosition]:Mode = Progress.Reflect.ParameterMode:Input then pEntity:GetPublicParameterName(oMethodParameters[iPosition]:name) else ""
                                .
                                
        end.     
        
        do while oIterator:HasNext():
            oMapEntry = cast(oIterator:Next(),IMapEntry).
            cName     = string(cast(oMapEntry:Key,String):Value).
            cValue    = cast(oMapEntry:Value,String):Value.
            iPosition = lookup(cname,cInternalInputNames).
          
            if iPosition = 0 then
               undo, throw new BadRequestError(subst("&1 is not a valid parameter for &2",cName,pMethod:Name)).
            
            if oMethodParameters[iPosition]:extent = 0 then
            do:
                cDataType = oMethodParameters[iPosition]:DataType:ToString().  
                
                case oMethodParameters[iPosition]:DataType:
                    when Progress.Reflect.DataType:Character then
                        pParameterList:SetParameter(iPosition,cDataType,"input", string(cvalue)).
                    when Progress.Reflect.DataType:Longchar then 
                        pParameterList:SetParameter(iPosition,cDataType,"input", cValue).
                    when Progress.Reflect.DataType:Integer then
                        pParameterList:SetParameter(iPosition,cDataType,"input", int(cValue)).
                    when Progress.Reflect.DataType:Int64 then
                        pParameterList:SetParameter(iPosition,cDataType,"input", int64(cValue)).
                    when Progress.Reflect.DataType:Decimal then
                        pParameterList:SetParameter(iPosition,cDataType,"input", dec(cValue)).
                    when Progress.Reflect.DataType:Logical then
                        pParameterList:SetParameter(iPosition,cDataType,"input", logical(cValue)).
                    when Progress.Reflect.DataType:Date then
                        pParameterList:SetParameter(iPosition,cDataType,"input", date(cValue)).
                    when Progress.Reflect.DataType:Datetime then
                        pParameterList:SetParameter(iPosition,cDataType,"input", datetime(cValue)).
                    when Progress.Reflect.DataType:Datetimetz then
                        pParameterList:SetParameter(iPosition,"datetime-tz","input", datetime-tz(cValue)).
                          
                end case.
            end.
            else 
               SetArrayParameter(pParameterList,oMethodParameters[iPosition],iPosition,cValue).
            
        end. 
       
    end method.
     
    method private final void SetArrayParameter(pParameterList as ParameterList, pParameter as Parameter, pPosition as integer,pcValue as longchar):
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
        define variable cDataType as character no-undo.
        define variable iPosition as integer no-undo.
        oJsonParser = new ObjectModelParser().
        pcValue = codepage-convert (pcValue,"utf-8").
            
        do on error undo, throw:
            oJsonArray = cast(oJsonParser:Parse(pcValue),JsonArray).
            catch e as Progress.Lang.Error :
                undo, throw new BadRequestError(subst("Parameter '&1' value &2 is not a valid array",pParameter:name,pcValue),e).      
            end catch. 
        end.
        if oJsonArray:length > 0 then
        do:
            case pParameter:DataType:
                when Progress.Reflect.DataType:LongChar then 
                    extent(cLongs) = oJsonArray:length.
                when Progress.Reflect.DataType:Character then 
                    extent(cVals) = oJsonArray:length.
                when Progress.Reflect.DataType:Logical then 
                    extent(lVals) = oJsonArray:length.
                when Progress.Reflect.DataType:Integer then 
                    extent(iInts) = oJsonArray:length.
                when Progress.Reflect.DataType:Int64 then 
                    extent(iInt64s) = oJsonArray:length.
                when Progress.Reflect.DataType:Decimal  then 
                    extent(dVals) = oJsonArray:length.
                when Progress.Reflect.DataType:Date then 
                    extent(dDates) = oJsonArray:length.
                when Progress.Reflect.DataType:Datetime then 
                    extent(dDateTimes) = oJsonArray:length.
                when Progress.Reflect.DataType:DateTimeTZ then 
                    extent(dDateTimeTzs) = oJsonArray:length.
                otherwise 
                    undo, throw new IllegalStateError(subst("Parameter &1 DataType '&2' is unsupported or invalid",pParameter:name,pParameter:DataType)).       
            end.    
        end.
        do i = 1 to oJsonArray:length
        on error undo, throw:
            case oJsonArray:GetType(i):
                when JsonDataType:NUMBER then
                do:
                    case pParameter:DataType:
                        when Progress.Reflect.DataType:Integer then
                            iInts[i] = oJsonArray:GetInteger(i).   
                        when Progress.Reflect.DataType:Int64 then 
                            iInt64s[i] = oJsonArray:GetInt64(i).   
                          when Progress.Reflect.DataType:Decimal  then 
                            dVals[i] = oJsonArray:GetDecimal(i).
                        otherwise do:
                            ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                        end.       
                    end case.
                end.
                when JsonDataType:BOOLEAN then
                do:
                    case pParameter:DataType:
                         when Progress.Reflect.DataType:Logical then 
                            lVals[i] = oJsonArray:GetLogical(i).   
                        otherwise do:
                            ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                        end.       
                    end case.
                end.
                when JsonDataType:STRING then
                do: 
                    case pParameter:DataType:
                        when Progress.Reflect.DataType:Character then 
                            cVals[i] = oJsonArray:GetCharacter(i).   
                        when Progress.Reflect.DataType:Longchar then 
                            cLongs[i] = oJsonArray:GetLongchar(i).   
                        when Progress.Reflect.DataType:Date then 
                            dDates[i] = oJsonArray:GetDate(i).
                        when Progress.Reflect.DataType:DateTime then 
                            dDateTimes[i] = oJsonArray:GetDatetime(i).
                        when Progress.Reflect.DataType:DateTimeTZ then 
                            dDateTimeTzs[i] = oJsonArray:GetDatetimeTZ(i).
                        otherwise do:
                            ThrowDataTypeError(pParameter,JsonUtil:GetStringValue(oJsonArray, i)).    
                        end.       
                    end.
                end.     
                when JsonDataType:NULL then
                do: 
                    case pParameter:DataType:
                        when Progress.Reflect.DataType:Integer then
                            iInts[i] = ?.   
                        when Progress.Reflect.DataType:Int64 then 
                            iInt64s[i] = ?.   
                        when Progress.Reflect.DataType:Decimal  then
                            dVals[i] = ?.
                        when Progress.Reflect.DataType:Logical then
                            lVals[i] = ?.
                        when Progress.Reflect.DataType:Character then
                            cVals[i] = ?.   
                       // these are not really needed since ? is defaailt for longchar and dates      
                        when Progress.Reflect.DataType:LongChar then 
                            cLongs[i] = ?.   
                        when Progress.Reflect.DataType:Date  then
                            dDates[i] = ?.
                        when Progress.Reflect.DataType:Datetime  then
                            dDateTimes[i] = ?.
                        when Progress.Reflect.DataType:DatetimeTZ then
                            dDateTimeTzs[i] = ?.       
                   end.         
                end.
                otherwise
                    undo, throw new BadRequestError(subst("Parameter &1 value is not a primitive value",pParameter:name)).   
  
            end case.
            catch e as Progress.Lang.Error :
                undo, throw new BadRequestError(subst("Attempting to set &1 parameter &2[3] to &4",
                                                       // entry 1 to not show extent in text if present (we show the number in bracket)
                                                       pParameter:DataType:ToString(),
                                                       pParameter:name,
                                                       i,
                                                       JsonUtil:GetStringValue(oJsonArray, i))
                                                       ,e).    
                                                               
            end catch. 
        end.
        cDataType = pParameter:DataType:ToString() + " extent".
        case pParameter:DataType:
            when  Progress.Reflect.DataType:Integer then
                pParameterList:SetParameter(pPosition,cDataType,"input", iInts).
            when  Progress.Reflect.DataType:Int64 then 
                pParameterList:SetParameter(pPosition,cDataType,"input", iInt64s).
            when  Progress.Reflect.DataType:Decimal  then
                pParameterList:SetParameter(pPosition,cDataType,"input", dVals).
            when  Progress.Reflect.DataType:Logical then
                pParameterList:SetParameter(pPosition,cDataType,"input", lVals).
            when  Progress.Reflect.DataType:Character then
                pParameterList:SetParameter(pPosition,cDataType,"input", cvals).
           // these are not really needed since ? is defaailt for longchar and dates      
            when  Progress.Reflect.DataType:LongChar then 
                pParameterList:SetParameter(pPosition,cDataType,"input", cLongs).
            when  Progress.Reflect.DataType:Date  then
                pParameterList:SetParameter(pPosition,cDataType,"input", dDates).
            when  Progress.Reflect.DataType:Datetime  then
                pParameterList:SetParameter(pPosition,cDataType,"input", dDateTimes).
            when  Progress.Reflect.DataType:DatetimeTZ  then
                pParameterList:SetParameter(pPosition,cDataType,"input", dDateTimeTzs).
          end case.
        
    end method.    
   
     method protected void ThrowDataTypeError(pParam as Parameter,  pcValue as longchar):
        undo, throw new BadRequestError(subst("Invalid value &1 for parameter '&2'. The value is not a valid ABL '&3' data-type value.",
                                              pcValue, 
                                              pParam:name,
                                              // entry 1 to not show extent if present
                                              pParam:DataType
                                             )
                                       ).
    end method.
    
   
    method public void PrepareOutputParameters(pParameters as Parameter extent,pParameterList  as ParameterList).
        
        define variable iPosition as integer no-undo.
        empty temp-table ttOutput. 
        
        do iPosition = 1 to extent(pParameters):
            if pParameters[iPosition]:Mode = Progress.Reflect.ParameterMode:output then
            do: 
                create ttOutput.
                ttOutput.position = iPosition.
                case pParameters[iPosition]:DataType:
                    when Progress.Reflect.DataType:Character then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.characterValue).
                    
                    when Progress.Reflect.DataType:Longchar then 
                       // does this work?? 
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.clobValue).
                    
                    when Progress.Reflect.DataType:Integer then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.integerValue).
               
                    when Progress.Reflect.DataType:Int64 then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.int64Value).
                    
                    when Progress.Reflect.DataType:Decimal then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.decimalValue).
                         
                    when Progress.Reflect.DataType:Logical then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.logicalValue).
                    
                    when Progress.Reflect.DataType:Date then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.DateValue).
                    
                    when Progress.Reflect.DataType:Datetime then
                        pParameterList:SetParameter(iPosition,pParameters[iPosition]:DataType:ToString(),"output", ttOutput.DateTimeValue).
                        
                    when Progress.Reflect.DataType:Datetimetz then
                        pParameterList:SetParameter(iPosition,"datetime-tz","output", ttOutput.dateTimeTzValue).
                  
                end case.
            end.    
        end.        
    end method.   
    
    method protected void AddOutputValuesToJson(pServiceEntity as IServiceEntity,pMethod as Method,pJson as JsonObject).
        define variable oParams   as Parameter extent no-undo.
        define variable iPosition as integer no-undo.
        define variable cLongValue as longchar no-undo.
        oParams   = pMethod:GetParameters().
        do iPosition = 1 to extent(oParams):
            if oParams[iPosition]:Mode = Progress.Reflect.ParameterMode:output then
            do: 
                find ttOutput where ttOutput.position = iPosition no-error.
                if not avail ttOutput then 
                    undo, throw new IllegalArgumentError(subst("&1 does not have an output parameter in position &2",pMethod:name,iPosition)).
                     
                case oParams[iPosition]:DataType:
                    when Progress.Reflect.DataType:Character then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.characterValue).
                    when Progress.Reflect.DataType:Longchar then 
                    do:
                        copy-lob ttOutput.clobValue to cLongValue.
                       // does this work?? 
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),cLongValue).
                    end.
                    when Progress.Reflect.DataType:Integer then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.integerValue).
                    
                    when Progress.Reflect.DataType:Int64 then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.int64Value).
                    
                    when Progress.Reflect.DataType:Decimal then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.decimalValue).
                         
                    when Progress.Reflect.DataType:Logical then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.logicalValue).
                    
                    when Progress.Reflect.DataType:Date then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.dateValue).
                    
                    when Progress.Reflect.DataType:Datetime then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.datetimeValue).
                        
                    when Progress.Reflect.DataType:Datetimetz then
                        pJson:Add(pServiceEntity:GetPublicParameterName(oParams[iPosition]:name),ttOutput.datetimetzValue).
                    
                end case.
            end.    
        end.     
         
    end method.     
    
    method protected void InvokeAndAddValueToJson(pServiceEntity as IServiceEntity,
                                                  pMethod as Method,
                                                  pParameterList as ParameterList,
                                                  pJson as JsonObject,
                                                  pResponseNodeName as character ):
        define variable cVal as character no-undo.
        define variable iVal as integer no-undo.
        define variable i64val as int64 no-undo.
        define variable decVal as decimal no-undo.
        define variable datVal as date no-undo.
        define variable dtiVal as datetime no-undo.
        define variable dtzVal as datetime-tz no-undo.
        define variable lval as logical no-undo.
        define variable longval as longchar no-undo.    
        
        case pMethod:ReturnType:
            when Progress.Reflect.DataType:Character then do:
                cVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,cVal).
            end.    
            when Progress.Reflect.DataType:Date then do:
                datVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,datVal).
            end.    
            when Progress.Reflect.DataType:Datetime then do:
                dtiVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,dtiVal).
            end.    
            when Progress.Reflect.DataType:DateTimeTZ then do:
                dtzVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,dtzVal).
            end.    
            when Progress.Reflect.DataType:Decimal then do:
                decVal =  pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,decVal).
            end.
            when Progress.Reflect.DataType:Integer then do:
                iVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,iVal).
            end.
            when Progress.Reflect.DataType:Int64 then do:
                i64Val = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,i64Val).
            end.    
            when Progress.Reflect.DataType:Logical then do:
                lVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,lVal).
            end.    
            when Progress.Reflect.DataType:Longchar then do:
                longVal = pMethod:Invoke(pServiceEntity,pParameterList).
                pJson:Add(pResponseNodeName,longVal).
            end.    
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Get ServiceEntity method with datatype '&1'",pMethod:ReturnType:toString())).
        end case.         
        
    end method.
    
    method protected void InvokeAndAddExtentValuesToJson(pServiceEntity as IServiceEntity,
                                                     pMethod as Method,
                                                     pParameterList as ParameterList,
                                                     pJson as JsonObject,
                                                     pResponseNodeName as character):
        define variable cVal    as character   extent no-undo.
        define variable iVal    as integer     extent no-undo.
        define variable i64val  as int64       extent no-undo.
        define variable decVal  as decimal     extent no-undo.
        define variable datVal  as date        extent no-undo.
        define variable dtiVal  as datetime    extent no-undo.
        define variable dtzVal  as datetime-tz extent no-undo.
        define variable lval    as logical     extent no-undo.
        define variable longval as longchar    extent no-undo.    
        
        define variable oJsonArray as JsonArray no-undo.
                      
        oJsonArray = new JsonArray().     
        pJson:Add(pResponseNodeName,oJsonArray).  
        
        case pMethod:ReturnType:
            when Progress.Reflect.DataType:Character then do:
                cVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(cVal).
            end.    
            when Progress.Reflect.DataType:Date then do:
                datVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(datVal).
            end.    
            when Progress.Reflect.DataType:Datetime then do:
                dtiVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(dtiVal).
            end.    
            when Progress.Reflect.DataType:DateTimeTZ then do:
                dtzVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(dtzVal).
            end.    
            when Progress.Reflect.DataType:Decimal then do:
                decVal =  pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(decVal).
            end.
            when Progress.Reflect.DataType:Integer then do:
                iVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(iVal).
            end.
            when Progress.Reflect.DataType:Int64 then do:
                i64Val = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(i64Val).
            end.    
            when Progress.Reflect.DataType:Logical then do:
                lVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(lVal).
            end.    
            when Progress.Reflect.DataType:Longchar then do:
                longVal = pMethod:Invoke(pServiceEntity,pParameterList).
                oJsonArray:Add(longVal).
            end.    
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Get ServiceEntity method with datatype '&1'",pMethod:ReturnType:toString())).
        end case.         
        
    end method.
      
end class.
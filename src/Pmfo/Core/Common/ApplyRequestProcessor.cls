 
 /*------------------------------------------------------------------------
    File        : ApplyRequestProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Feb 11 09:38:58 EST 2023
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Core.Common.Aggregate from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.BusinessLogic.Map.EntityFieldMapHolder from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Common.AggregationMethodEnum from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.ApplyRequestProcessor: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
     ------------------------------------------------------------------------------*/
    
    define protected property FieldMapHolder as EntityFieldMapHolder no-undo 
        get():
            if not valid-object(FieldMapholder) then 
                FieldMapholder = new EntityFieldMapholder().
            return FieldMapholder.     
        end. 
        protected set.
    
    constructor public ApplyRequestProcessor (  ):
        super ().
    end constructor.
   
    method public JsonObject ProcessApply(pClientGetRequest as IClientGetRequest, phDataset as handle, phRequestedBuffer as handle):
         
        define variable oJsonObject      as JsonObject   no-undo.
        define variable oJsonValueArray  as JsonArray   no-undo.
        define variable oJsonValue       as JsonObject   no-undo.
        define variable oAggregates      as ICollection  no-undo.
        define variable oAggregate       as Aggregate  no-undo.
        define variable oGroups          as character    no-undo.
        define variable hQuery           as handle       no-undo.
        define variable cGroups          as character no-undo.
        define variable cGroup           as character no-undo.
        define variable iGroup           as integer no-undo.
        define variable oIterator        as IIterator no-undo.
        define variable cGroupfield      as character no-undo.
        define variable cAggregateField  as character no-undo.
        define variable hField           as handle no-undo.
        define variable iCountField      as integer no-undo.
        define variable hAggregateFields as handle                extent no-undo.
        define variable oAggregateModes  as AggregationMethodEnum extent no-undo.
        define variable cAggregateNames  as character             extent no-undo.
        define variable iErrorAggregate  as integer no-undo.  
        define variable iCount           as integer               no-undo.
        define variable dSum             as decimal               extent no-undo.
        define variable dMin             as decimal               extent no-undo.
        define variable dMax             as decimal               extent no-undo.
        define variable dMinDate         as date                  extent no-undo.
        define variable dMaxDate         as date                  extent no-undo.
        define variable dMinDateTime     as datetime              extent no-undo.
        define variable dMaxDateTime     as datetime              extent no-undo.
        define variable dMinDateTimetz   as datetime-tz           extent no-undo.
        define variable dMaxDateTimetz   as datetime-tz           extent no-undo.
        define variable iAggregate       as integer no-undo. 
        define variable lWriteAll        as logical no-undo.
        define variable cNames           as character extent no-undo.
        create query hQuery.
        
        
        hQuery:add-buffer(phRequestedBuffer).
        cGroups = pClientGetRequest:GroupBy.
        
        
        if pClientGetRequest:HasAggregate then
            oAggregates = pClientGetRequest:Aggregates.
       
        oJsonObject = new JsonObject().
        oJsonValueArray  = new JsonArray().
        oJsonObject:Add("value",oJsonValueArray).
        
        if valid-object(oAggregates) then 
        do: 
            
            extent(hAggregateFields) = oAggregates:size. 
            extent(oAggregateModes)  = oAggregates:size.
            extent(cAggregateNames)  = oAggregates:size.
            extent(dSum)  = oAggregates:size.
            extent(dMin)  = oAggregates:size.
            extent(dMax)  = oAggregates:size.
            extent(dMinDate)  = oAggregates:size.
            extent(dMaxDate)  = oAggregates:size.
            extent(dMinDatetime)  = oAggregates:size.
            extent(dMaxDatetime)  = oAggregates:size.
            extent(dMinDatetimeTz)  = oAggregates:size.
            extent(dMaxDatetimeTz)  = oAggregates:size.
            
            oIterator = oAggregates:Iterator().
            do while oIterator:HasNext():
                iAggregate = iAggregate + 1. 
                oAggregate = cast(oIterator:Next(),Aggregate).
                if oAggregate:PublicFieldName = "count" and not valid-object(oAggregate:AggregateMethod) then 
                    iCountField = iAggregate.
                else 
                do:    
                    cAggregateField =  GetUseFieldName(phRequestedBuffer, oAggregate:PublicFieldName).
                    hAggregateFields[iAggregate] = phRequestedBuffer:buffer-field(cAggregateField) no-error.  
                    if not valid-handle(hAggregateFields[iAggregate]) then 
                        undo, throw new BadRequestError(subst("Aggregate '&1' is not a valid property in &2",cAggregateField,phRequestedBuffer:serialize-name)).
                    case oAggregate:AggregateMethod:
                        when AggregationMethodEnum:Average or when AggregationMethodEnum:Sum then 
                            if lookup(hAggregateFields[iAggregate]:data-type,"integer,int64,decimal") = 0 then
                                undo, throw new IllegalArgumentError(subst("(Invalid Aggregate expression '&1'. '&2' must be a numeric field in order to aggregate with '&3'.",
                                                                    oAggregate:toString(),
                                                                    oAggregate:PublicFieldName,
                                                                    oAggregate:AggregateMethod:ToString()                 
                                                             )
                                                            ). 
                        when AggregationMethodEnum:Max or when AggregationMethodEnum:Min then 
                            if lookup(hAggregateFields[iAggregate]:data-type,"integer,int64,decimal,date,datetime,datatime-tz") = 0 then
                                undo, throw new IllegalArgumentError(subst("(Invalid Aggregate expression '&1'. '&2' must be a numeric, date, date time or date time tz field in order to aggregate with '&3'.",
                                                                    oAggregate:toString(),
                                                                    oAggregate:PublicFieldName,
                                                                    oAggregate:AggregateMethod:ToString()                 
                                                             )
                                                            ). 
                    
                    end.                                            
                    oAggregateModes[iAggregate] = oAggregate:AggregateMethod.
                end.
                cAggregateNames[iAggregate] = oAggregate:SerializeName.
            end.   
        end.
        
        do iGroup = 1 to max(num-entries(cGroups),1):
            if valid-object(oAggregate) then
            do:
                assign
                    iCount = 0
                    dMax   = ?
                    dMax   = ?
                    dMinDate = ?
                    dMaxDate = ?
                    dMinDatetime = ?
                    dMaxDatetime = ?
                    dMinDatetimetz = ?
                    dMaxDatetimetz = ?
                    dSum   = ? // return null if no non-null values are found
                    .
            end.    
          
            if cGroups > "" then
            do:  
                cGroup = entry(iGroup,CGroups).
                
                cGroupfield = GetUseFieldName(phRequestedBuffer, cGroup).
                hField = phRequestedBuffer:buffer-field(cGroupfield) no-error.  
                if not valid-handle(hField) then 
                    undo, throw new BadRequestError(subst("Groupby '&1' is not a valid property in &2",cGroup,phRequestedBuffer:serialize-name)).  
                        
                hQuery:query-prepare(subst("for each &1 break by &2",phRequestedBuffer:name,cGroupfield)).
            end.
            else
                hQuery:query-prepare(subst("for each &1",phRequestedBuffer:name)).
            
            hQuery:query-open().
            hQuery:get-first.
            if phRequestedBuffer:avail = false and cGroups = "" then 
                lWriteAll = true. 
            do while true: //hBuffer:avail
                if phRequestedBuffer:avail then 
                do:
                    iCount = iCount + 1.
                    do iAggregate = 1 to extent(hAggregateFields): 
                        if iAggregate <> iCountField then
                        do:
                            if hAggregateFields[iAggregate]:buffer-value <> ? then
                            do:
                                case hAggregateFields[iAggregate]:data-type:
                                    when "integer" or when "int64"  or when "decimal" then
                                    do:
                                        if dSum[iAggregate] = ? then 
                                            dSum[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else                
                                            dSum[iAggregate] = dSum[iAggregate] + hAggregateFields[iAggregate]:buffer-value.
                                        
                                        if dMax[iAggregate] = ? then 
                                            dMax[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMax[iAggregate] = max(dMax[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                           
                                        if dMin[iAggregate] = ? then 
                                            dMin[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMin[iAggregate] = min(dMin[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                    end.
                                    when "date" then
                                    do:
                                        if dMaxDate[iAggregate] = ? then 
                                            dMaxDate[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMaxDate[iAggregate] = max(dMaxDate[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                           
                                        if dMinDate[iAggregate] = ? then 
                                            dMinDate[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMinDate[iAggregate] = min(dMinDate[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                    end.
                                    when "datetime" then
                                    do:
                                        if dMaxDatetime[iAggregate] = ? then 
                                            dMaxDatetime[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMaxDatetime[iAggregate] = max(dMaxDatetime[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                           
                                        if dMinDatetime[iAggregate] = ? then 
                                            dMinDatetime[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMinDatetime[iAggregate] = min(dMinDatetime[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                    end. 
                                    when "datetime-tz" then
                                    do:
                                        if dMaxDatetimeTz[iAggregate] = ? then 
                                            dMaxDatetimeTz[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMaxDatetimeTz[iAggregate] = max(dMaxDatetimeTz[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                           
                                        if dMinDatetimeTz[iAggregate] = ? then 
                                            dMinDatetimeTz[iAggregate] = hAggregateFields[iAggregate]:buffer-value.
                                        else
                                            dMinDatetimeTz[iAggregate] = min(dMinDatetimeTz[iAggregate],hAggregateFields[iAggregate]:buffer-value).
                                    end. 
                                end.    
                            end.
                        end.    
                    end.
                end.    
                
                if (cGroups > "" and hQuery:last-of (1)) 
                or lWriteAll then 
                do: 
                    oJsonValue = new JsonObject().    
                    if valid-object(hField) and phRequestedBuffer:avail then
                        oJsonValue:Add(cGroup,hField:buffer-value).
                    
                    do iAggregate = 1 to extent(hAggregateFields)
                    on error undo, throw: 
                        if iAggregate = iCountField then 
                             oJsonValue:Add(cAggregateNames[iAggregate],iCount).
                        else do: 
                             case oAggregateModes[iAggregate]:  
                                 when AggregationMethodEnum:Max then
                                 do: 
                                     case hAggregateFields[iAggregate]:data-type: 
                                         when "decimal" then
                                         do:   
                                             if dMax[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMax[iAggregate]).
                                         end.
                                         when "integer" or when "int64" then
                                         do:
                                             if dMax[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else    
                                                 oJsonValue:Add( cAggregateNames[iAggregate],int64(dMax[iAggregate])).
                                         end.    
                                         when "date" then
                                         do:   
                                             if dMaxDate[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else    
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMaxDate[iAggregate]).
                                         end.     
                                         when "datetime" then
                                         do:   
                                             if dMaxDateTime[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else 
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMaxDatetime[iAggregate]).
                                         end.     
                                         when "datetime-tz" then
                                         do:   
                                             if dMaxDateTimeTz[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMaxDatetimeTz[iAggregate]).
                                         end.
                                     end case.
                                 end.
                                 when AggregationMethodEnum:Min then 
                                 do:
                                     case hAggregateFields[iAggregate]:data-type: 
                                         when "decimal" then
                                         do:   
                                             if dMin[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMin[iAggregate]).
                                         end.
                                         when "integer" or when "int64" then
                                         do:
                                             if dMin[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else    
                                                 oJsonValue:Add( cAggregateNames[iAggregate],int64(dMin[iAggregate])).
                                         end.    
                                         when "date" then
                                         do:   
                                             if dMinDate[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else    
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMinDate[iAggregate]).
                                         end.     
                                         when "datetime" then
                                         do:   
                                             if dMinDateTime[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else 
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMinDatetime[iAggregate]).
                                         end.     
                                         when "datetime-tz" then
                                         do:   
                                             if dMinDateTimeTz[iAggregate] = ? then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dMinDatetimeTz[iAggregate]).
                                         end.
                                     end case.
                                       
                                 end.    
                                 when AggregationMethodEnum:Sum then 
                                 do:
                                     if dSum[iAggregate] = ? then 
                                         oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                     else do: 
                                         case hAggregateFields[iAggregate]:data-type: 
                                             when "decimal" then 
                                                  oJsonValue:Add(cAggregateNames[iAggregate],dSum[iAggregate]).
                                             when "integer" or when "int64" then
                                                 oJsonValue:Add( cAggregateNames[iAggregate],int64(dSum[iAggregate])).
                                         end case.
                                     end.
                                 end.    
                                 when AggregationMethodEnum:Average then 
                                 do:
                                     case hAggregateFields[iAggregate]:data-type: 
                                         when "decimal" or when "integer" or when "int64" then
                                         do:
                                             if dSum[iAggregate] = ? or iCount = 0 then 
                                                 oJsonValue:AddNull(cAggregateNames[iAggregate]).
                                             else // average of in may satill be decimal (UI need to deal with this if necessary )
                                                 oJsonValue:Add(cAggregateNames[iAggregate],dSum[iAggregate] / iCount).
                                         end.
                                     end case.
                                 end.
                                 when AggregationMethodEnum:CountDistinct then 
                                 do: 
                                     undo, throw new NotImplementedError(subst("Aggregate &1",AggregationMethodEnum:CountDistinct:ToString())). 
                                 end.    
                             end case.
                         end.
                         catch e as Progress.Lang.Error :
                             if e:GetMessageNum(1) = 16056 then 
                             do: 
                                 oIterator = oAggregates:Iterator().
                                 do while oIterator:HasNext():
                                     iErrorAggregate = iErrorAggregate + 1. 
                                     oAggregate = cast(oIterator:Next(),Aggregate).
                                     undo, throw new IllegalArgumentError(subst("(Invalid Aggregate expression '&1'. Name '&2' is already defined as aggregate alias.",
                                                                                  oAggregate:toString(),
                                                                                  oAggregate:SerializeName
                                                                                )
                                                                          ,e). 
                                 end.
                             end.    
                             undo, throw e.   
                         end catch.
                    end.
                    
                    cNames = oJsonValue:GetNames().
                    if extent(cNames) > 0 then
                        oJsonValueArray:Add(oJsonValue).
                      
                    if valid-object (oAggregate) then
                    do:
                        assign
                            iCount   = 0
                            dSum     = ?
                            dMax     = ?
                            dMin     = ?
                            dMinDate = ?
                            dMaxDate = ?
                            dMinDatetime   = ?
                            dMaxDatetime   = ?
                            dMinDatetimetz = ?
                            dMaxDatetimetz = ?
                            .
                    end.
                end.
                
                if lWriteAll then 
                    leave.
                    
                hQuery:get-next().
               
                if phRequestedBuffer:avail = false then
                do: 
                    if cGroups > "" then 
                        leave.
                    else
                        lWriteAll = true. // one more loop to write out the resilts     
                end.     
            end.
        end.
                    
        return oJsonObject. 
        finally:
             if valid-object(hQuery) then 
                 delete object hQuery.     
        end finally. 
    end method.    
    
    
    method protected character GetUseFieldName(phBuffer as handle,pcfield as characte):
        define variable cMap      as character no-undo.
        define variable cUseField as character no-undo.
        cMap = FieldMapHolder:GetFieldMap(phBuffer).
        return StringUtil:GetUseName(pcField, cMap) .
    end method.  
    

end class.
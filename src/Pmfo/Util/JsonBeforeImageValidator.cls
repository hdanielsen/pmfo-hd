 
 /*------------------------------------------------------------------------
    File        : JsonBeforeImageValidator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jan 17 06:24:58 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Util.JsonParser from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.JsonParser from propath.
using Pmfo.Util.BufferUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Util.Array from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.

class Pmfo.Util.JsonBeforeImageValidator inherits JsonParser: 
    define protected property BeforeProperty  as character init "prods:before" no-undo get.
    define protected property HasChangesProperty     as character init "prods:hasChanges" no-undo get.
    define protected property RowStateProperty  as character init "prods:rowState" no-undo get.
    define protected property IdProperty  as character init "prods:id" no-undo get.
    define private variable BeforeImageSection as logical no-undo.
    define private variable CurrentArray       as character no-undo. 
    define private variable OnlyCheckTopAfterRow as logical no-undo.
    define private variable RequestedBuffer as handle no-undo.
    define private variable WebMethod as character no-undo.
     
    define temp-table ttAfter 
        field name as character
        field rowstate as integer
        field key as character
        index key as primary unique key.
    
    define temp-table ttBefore
        field name as character 
        field key as character
        field rowstate as integer
        index key as primary unique key.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public JsonBeforeImageValidator (pcWebMethod as character  ):
        super ().
        WebMethod = pcWebMethod.
    end constructor.

    method public void Parse( input pjsonObject as JsonObject, plOnlyTopAfterRow as logical, phBuffer as handle ):
        OnlyCheckTopAfterRow = plOnlyTopAfterRow.
        Requestedbuffer = phbuffer.
    
        Parse(pJsonObject).
    end method.
    
    method override public void Parse( input pjsonObject as JsonObject ):
        define variable cNames   as character extent no-undo.
        define variable cTopName as character no-undo.
        
        cNames = pJsonObject:GetNames().
        // if dataset skip top 
        if extent(cNames) = 1 and pJsonObject:GetType(cNames[1]) = JsonDataType:Object then
        do:
            pJsonObject =  pJsonObject:GetJsonObject(cNames[1]).
        end.     
   
        if pjsonObject:Has(HasChangesProperty) then 
        do:
            if pJsonObject:GetLogical(HasChangesProperty) = false then 
                undo, throw new IllegalStateError(subst('Property "&1" is false',HasChangesProperty)).
        end.
        else 
            undo, throw new IllegalStateError(subst('Property "&1" is missing',HasChangesProperty)).
           
        super:Parse(input pjsonObject).
        
        for each ttAfter where ttAfter.rowstate <> row-created:
        
            find ttBefore where ttBefore.key = ttAfter.key  no-error.
            if not avail ttBefore then 
               undo, throw new UnprocessableEntityError(subst('Before image row for &1 with prods:id "&2" is missing',
                                                                     ttAfter.name,
                                                                     ttAfter.key
                                                             )
                                                        ). 
                                                                     
           delete ttBefore.         
        end.
        
        find first ttBefore where ttBefore.rowstate <> row-deleted no-error. 
        if avail ttBefore then
            undo, throw new UnprocessableEntityError(subst('Before image Row for &1 with prods:id "&2" does not have a corresponding after row',
                                                                     ttBefore.name,
                                                                     ttBefore.key
                                                             )
                                                        ).
         
    end method.    
    
    method override public void Parse( input pjsonObject as JsonObject,pcName as character ):
        define variable iState as integer no-undo.
        define variable cKey as character no-undo.
        define variable cNames as character extent no-undo.
        define variable lBefore as logical no-undo.
        define variable cUnsupportedMethodTemplate as character init '"&1" property value "&2" for "&3" &4 row is not supported for "&5" method' no-undo.
        // name = current means we are in an unamed object directly in the array whihc are the only object allowed in before image
        if pcname <> CurrentArray and BeforeImageSection then 
            undo, throw new UnprocessableEntityError(subst('JSON Object "&1" under the "&2" array is not valid. The arrays in the "&3" section cannot have child objects.',                                                                           
                                                            pcName,   
                                                            CurrentArray,    
                                                            BeforeProperty)
                                                          ).
                 
            
        if pcName = BeforeProperty then
        do: 
            BeforeImageSection = true.
            lBefore = true.
        end.    
        else if pcName > "" then
        do:
             cNames = pjsonObject:GetNames().
             iState = ?.
             if pJsonObject:Has(RowStateProperty) then
             do:
                 iState = JsonUtil:GetJsonRowState(pJsonObject).
                 case iState:
                     when 0 then 
                         undo, throw new UnprocessableEntityError(subst('Cannot process JSON for update. "&1" property value "&2" for "&3" &4 row is not valid',
                                                                        RowStateProperty,
                                                                        pJsonObject:GetCharacter(RowStateProperty),
                                                                        pcname,
                                                                        if BeforeImageSection then "before image " else ""
                                                                        )
                                                                ).
                     when row-created then
                     do: 
                         if WebMethod <> "POST" then
                             undo, throw new UnsupportedOperationError(subst(cUnsupportedMethodTemplate,
                                                                             RowStateProperty,
                                                                             pJsonObject:GetCharacter(RowStateProperty),
                                                                             pcname,
                                                                             if BeforeImageSection then "before image " else "",
                                                                             WebMethod
                                                                            )
                                                                        ).

                     end.
                     when row-deleted then
                     do: 
                         if WebMethod <> "POST" and WebMethod <> "DELETE" then
                             undo, throw new UnsupportedOperationError(subst(cUnsupportedMethodTemplate,
                                                                             RowStateProperty,
                                                                             pJsonObject:GetCharacter(RowStateProperty),
                                                                             pcname,
                                                                             if BeforeImageSection then "before image " else "",
                                                                             WebMethod
                                                                            )
                                                                        ).
                     end.
                     when row-modified then
                     do:
                         if WebMethod <> "POST" and WebMethod <> "PUT" then
                                undo, throw new UnsupportedOperationError(subst(cUnsupportedMethodTemplate,
                                                                             RowStateProperty,
                                                                             pJsonObject:GetCharacter(RowStateProperty),
                                                                             pcname,
                                                                             if BeforeImageSection then "before image " else "",
                                                                             WebMethod
                                                                            )
                                                                        ).

                          
                     end.
                     
                 end.
             end.    
             else do:
                 if (BEforeImageSection = true // or OnlyCheckTopAfterRow = false
                 ) 
                 then
                     undo, throw new UnprocessableEntityError(subst('Cannot process JSON for update. "&1" property is missing for "&2" &3row',
                                                                    RowStateProperty,
                                                                    pcname,
                                                                    if BeforeImageSection then "before image " else ""
                                                                   )
                                                              ).
             end.    
             if pJsonObject:Has(IdProperty) then
             do:
                 cKey = PJsonObject:GetCharacter(IdProperty).
                 if BeforeImageSection then 
                 do on error undo, throw:
                     create ttBefore.
                     ttBefore.key = cKey.
                     ttBefore.name = pcname.
                     ttBefore.rowstate = iState.
                     validate ttBefore.
                     catch e as Progress.Lang.Error :
                         delete ttBefore.   
                         if e:GetMessageNum(1) = 132 then
                             undo, throw new UnprocessableEntityError(subst('Before image Row for "&1" prods:id value "&2" is a duplicate of the prods:id of another before image row',
                                                                     pcname,
                                                                     cKey
                                                                     )
                                                                ).
                        else 
                            undo, throw e.
                                                                
                     end catch.  
                 end.
                 else do on error undo, throw:   
                     if iState = ? then
                        undo, throw new UnprocessableEntityError(subst('Cannot process JSON for update. "&1" property is missing for "&2" row with "&4": "&5"',
                                                                    RowStateProperty,
                                                                    pcname,
                                                                    IdProperty,
                                                                    pjsonObject:GetCharacter(IdProperty)
                                                                   )
                                                              ).
                     create ttAfter.
                     ttAfter.key = cKey.
                     ttafter.name = pcname.
                     ttAfter.rowstate = istate.
                     validate ttAfter.
                     catch e as Progress.Lang.Error :
                         delete ttAfter.   
                         if e:GetMessageNum(1) = 132 then
                             undo, throw new UnprocessableEntityError(subst('Row for "&1" prods:id value "&2" is a duplicate of the prods:id of another row',
                                                                      pcname,
                                                                      cKey
                                                                     )
                                                                ).
                        else 
                            undo, throw e.
                                                                
                     end catch.  
                 end. 
             end.    
             else do:
                 if (BeforeImageSection = true or iState > 0
                 ) then
                    undo, throw new UnprocessableEntityError(subst('Cannot process JSON for update. "&1" property is missing for "&2" &3row with "&4": "&5"',
                                                                    IdProperty,
                                                                    pcname,
                                                                    if BeforeImageSection then "before image " else "",
                                                                    RowStateProperty,
                                                                    pjsonObject:GetCharacter(RowStateProperty)
                                                                   )
                                                              ).
             end.  
        end.        
        super:Parse(input pjsonObject,pcName).
        if lBefore then 
            BeforeImageSection = false. 
    end method.    
    
    method override public void Parse( input pjsonArray as JsonArray, pcName as character ):
        define variable cCurrentArray as character no-undo.
        define variable hFld as handle no-undo.
        // name = current means we are in an unamed object directly in the array
        if BeforeImageSection and CurrentArray > "" and  pcname <> CurrentArray then 
        do:
            
            if valid-handle(RequestedBuffer) and Currentarray = RequestedBuffer:serialize-name then 
            do: 
                hFLd = BufferUtil:GetFieldByPublicName(Requestedbuffer,pcname).
            end.      
            if not valid-handle(hFld) //or hFld:extent = 0 ??
            then 
                  undo, throw new UnprocessableEntityError(subst('JSON array "&1" under the "&2" array is not valid. The arrays in the "&3" section cannot have child  entity arrays.',                                                                           
                                                                pcName,   
                                                                CurrentArray,    
                                                                BeforeProperty)
                                                              ).
                     
        end.
        cCurrentArray = CurrentArray.
        CurrentArray = pcName.
     
        super:Parse(input pjsonArray,pcName).
        CurrentArray = cCurrentArray.
        
    end method.    
    
end class.
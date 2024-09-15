 
 /*------------------------------------------------------------------------
    File        : ErrorStringUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jun 10 07:25:06 EDT 2019
    Notes       : Support complex errors in buffer error string 
                  This should be used to add the error to the buffer 
                  and to extract the error from the buffer to json format 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.BadRequestError from propath.

block-level on error undo, throw.

class Pmfo.Util.ErrorStringUtil: 
    define private static property Dlm    as char no-undo get. set.
	define private static property FldDlm as char no-undo get. set.
    	
	constructor static ErrorStringUtil (  ):
	    Dlm = chr(3). 	
	    FldDLm = chr(4).
	end constructor.
    
    method public static void AddError (phBuffer as handle, pcMessage as character, pcField as char):
        AddBufferError(phBuffer, "Message", pcMessage, 0, pcField).     
    end method.

    method public static void AddQuestion (phBuffer as handle, piId as integer, pcMessage as character, pcField as char):
        AddBufferError(phBuffer, "Question", pcMessage, piId, pcField).
    end.    
    
    method private static void AddBufferError(phBuffer as handle, pcType as character, pcMessage as character, piNumber as integer, pcField as char):
        define variable cFieldSerializeName as character no-undo.
        define variable cValue as character no-undo.
        if pcMessage = "" or pcMessage = ? then
             undo, throw new IllegalArgumentError(subst("Error in error handling - &1 cannot be null or blank",if pcType <> "message" then subst("&1 message",pcType) else pcType)).
        
        if pcfield > "" then 
        do on error undo, throw:
            cFieldSerializeName = BufferUtil:GetFieldSerializeName(phBuffer,pcField).
            catch e as IllegalArgumentError:
                undo, throw new ApplicationError(subst("Error in error handling - &1",e:GetStrippedMessage()),e).    
            end catch.
        end.
        if phBuffer:row-state = 0 then
        do: 
            // hack 
            if pctype = "question" then 
            do:
                if not valid-object(phBuffer:table-handle:before-table) then 
                    undo, throw new ApplicationError(subst("Error in error handling - Cannot Ask Question for erad only table &1",phBuffer:name)).    
                
                // hack row modified without really chaning anything  
                phBuffer:table-handle:tracking-changes = true.
                cValue = phBuffer:buffer-field(1):buffer-value.
                phBuffer:buffer-field(1):buffer-value = ?. 
                phBuffer:buffer-field(1):buffer-value = cValue. 
                
                phBuffer:table-handle:tracking-changes = false.
                
            end.
            else 
                undo, throw new BadRequestError(pcMessage,piNumber).
                
        end.
        
        assign
            phBuffer:error         = true
            phBuffer:dataset:error = true when valid-handle(phBuffer:dataset) // not avail in cases when table passed by-reference
            phBuffer:error-string  = (if phBuffer:error-string = ? then "" else phBuffer:error-string) 
                                     + (if phBuffer:error-string > ""  then Dlm else "")
                                     + pcMessage
                                     + chr(4) + pcType  
                                     + chr(4) + string(piNumber) // message num can be added here 
                                     + if cFieldSerializeName > "" then chr(4) + cFieldSerializeName else "".
                                     
    end method.
    
   
    method public static void AddError (phBuffer as handle, pcMessage as character):
        AddError(phBuffer,pcMessage,"").
    end.    
    
    method public static void AddQuestion (phBuffer as handle, pid as integer, pcMessage as character):
        AddQuestion(phBuffer,pId,pcMessage,"").
    end.    
    
    method public static integer GetNumMessages (phBuffer as handle):
        return num-entries(phBuffer:error-string,Dlm).
    end.    
    
    method private static integer GetNumMessages (pcMessage as character):
        return num-entries(pcMessage,Dlm).
    end.    
    
    /* return errors from error-string TODO - return as objects
    */       
    method public static character extent GetErrors (phBuffer as handle, output pcTypes as character extent, output piIDs as integer extent, output pcFields as character extent):
        define variable cDummy as character extent no-undo.
        Assert:NotNull(phBuffer,"Buffer").
        if phBuffer:error then 
            return GetErrors(phBuffer:error-string, output pcTypes, output piIds, output pcFields).
        else
            return cDummy.    
    end.    
    
    /* return errors from error-string TODO - return as objects
    */       
    method public static character extent GetErrors (pcMessage as character, output pcTypes as character extent, output pids as integer extent,output pcFields as character extent):
        define variable i as integer no-undo.
        define variable iNum as integer no-undo.
        define variable cMessages as character extent no-undo.
        
        if pcMessage > "" then 
        do:
            extent(cMessages) = GetNumMessages(pcMessage).
            extent(pcTypes) = extent(cMessages).
            extent(pids) = extent(cMessages).
            extent(pcFields) = extent(cMessages).
            
            do i = 1 to extent(cMessages):
               cMessages[i] = entry(i,pcMessage,Dlm).
               if num-entries(cMessages[i],FLdDLm) > 1 then
               do:
                   assign
                       pcTypes[i] = entry(2,cMessages[i],FldDlm). 
                   if num-entries(cMessages[i],FLdDLm)>= 3 then
                   do:
                       pIds[i] = int(entry(3,cMessages[i],FldDlm)).
                       if num-entries(cMessages[i],FLdDLm) = 4 then
                           pcFields[i] = entry(4,cMessages[i],FldDlm).
                   end.
                   cMessages[i] = entry(1,cMessages[i],FldDlm).
               end.    
            end.
        end.
        return cMessages.    
    end.    
    
    method public static character extent GetErrors (pcMessage as character, output piIds as integer extent ):
        define variable cMessages as character extent no-undo.
        define variable cDummy as character extent no-undo.
        cMessages = GetErrors(pcMessage, output cDummy, output piIDs, output cDummy). 
        return cMessages.    
    end.    
    
    method public static character extent GetErrors (pcMessage as character ):
        define variable cMessages as character extent no-undo.
        define variable cDummy as character extent no-undo.
        define variable iDummy as integer extent no-undo.
        cMessages = GetErrors(pcMessage, output cDummy, output iDummy, output cDummy). 
        return cMessages.    
    end.    
    
    
    method public static void GetConfirmationRange(pcType as character, output piMin as integer, output piMax as integer):
        case pcType: 
            when "DataLayer" then
                assign 
                    piMin = 0
                    piMax = 100.
                    
            when "BusinessLayer" then
                assign 
                    piMin = 101
                    piMax = 200.
                    
            when "FieldValidation" then
                assign 
                    piMin = 201
                    piMax = ?.
            otherwise
                undo, throw new IllegalArgumentError(subst("Type &1",pcType)).         
        end.    
    end method.
    
  
end class.
 
 /*------------------------------------------------------------------------
    File        : UnconvertableAsciiCharacterUtil
    Purpose     : Handle json read to temp-table field of JSON utf-8 character for applications that still has iso8859-1
    Syntax      : Use catch when assigning field from jsin and catch error 12009 and call this
     
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jun 12 05:42:08 EDT 2023
    Notes       : Converts characters commonly pasted from other soruces like email, pdf and word to ascii equivalent   
                  Throws a nUnprocessableEntityError with more specific information about the posution and field name 
                  for for other unconvertable characters  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.ByteBucket from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Util.UnconvertableAsciiCharacterUtil:
     
    
    // see ascii names in https://www.ascii-code.com/ - get data from there if adding more conversions  
    define private property CONVERTABLE_BYTES                 as character init "145,146,147,148,150,151" no-undo get. 
   
    // this is a hack - there must be a way to decode the unicode encoded in utf-8 as "â€" and one more character one of which is 157 that cannot be typed unless the file here is saved as utf8      
    // the string defined a list of the thurd byte in UTF encoded after "â€". This works for these 6, NOTE: not sure if all unicodewsc have tghe same escape characters  
    define private property THIRD_UNICODE_IN_UTF8             as character init "152,153,156,157,147,148" no-undo get. 
    // the offical name of 150 is En dash and 151 is Em dash
    define private property CONVERTABLE_DESCRIPTIONS_ARRAY    as character extent 6 init ["Left single quotation mark","Right single quotation mark","Left double quotation mark","Right double quotation mark","Dash","Long dash"] no-undo get.    
    define private property CONVERT_DESCRIPTIONS_ARRAY        as character extent 6 init ["Single quote","Single quote","Double quote","Double quote","Hyphen (Minus)","Hyphen (Minus)"] no-undo get. 
    define private property CONVERT_BYTES_ARRAY              as character extent 6 init ["'","'",'"','"',"-","-"] no-undo get. 
    
    method public void HandleJsonString(pJson as JsonObject, phField as handle, output pcWarning as character):
        phField:buffer-value = ConvertJsonString(pJson,  BufferUtil:GetRowName(phfield:buffer-handle,true), phField:serialize-name, BufferUtil:GetFieldLabel(phfield), output pcWarning).
    end method.
    
    method public longchar ConvertJsonString(pJson as JsonObject, pcTableName as character, pcPropertyName as character, output pcWarning as character):
        return ConvertJsonString(pJson, pcTableName, pcPropertyName,pcPropertyName, pcWarning).
    end method.
    
    method public longchar ConvertJsonString(pJson as JsonObject, pcTableName as character, pcPropertyName as character, pcPropertyLabel as character, output pcWarning as character):
        define variable oByteBucket     as ByteBucket no-undo.
        define variable i               as integer    no-undo.
        define variable cConverted      as longchar   no-undo.
        define variable iByte           as integer    no-undo.
        define variable iSkippedBytes   as integer no-undo.
        define variable iConvertnum     as integer    no-undo.
        define variable cWarning        as character  no-undo.
        define variable iWarningcount   as integer    no-undo.
       
        Assert:IsType(pJson, get-class(JsonObject)).
        Assert:IsTrue(pJson:GetType(pcPropertyName) = JsonDataType:STRING,"Json is a String").
        
        oByteBucket = new ByteBucket().
        oByteBucket:putString(pjson:getlongchar(pcPropertyName,"utf-8")).
    
        
        if pcPropertyLabel = "" or pcPropertyLabel = ? then 
            pcPropertyLabel = pcPropertyName.
            
        do i = 1 to oByteBucket:size:
            iByte = oByteBucket:GetByte(i).
            
            if iByte gt 127 then 
            do: 
                iConvertnum = 0.
                if iByte = 226 then // first escape char for unicode
                do:
                    if i le oByteBucket:size + 2 and oByteBucket:GetByte(i + 1) = 128 then // euro sign is second in unicode
                    do:
                       iConvertnum = lookup(string(oByteBucket:GetByte(i + 2)),THIRD_UNICODE_IN_UTF8).
                       iSkippedBytes = iSkippedBytes + 2.
                       i = i + 2.
                    end.   
                end.
                else 
                   iConvertnum = lookup(string(iByte),CONVERTABLE_BYTES).
                  
                if iConvertNum > 0 then 
                do:
                    cConverted = cconverted + CONVERT_BYTES_ARRAY[iConvertnum].
                    cWarning = StringUtil:AppendEntry(
                                          cWarning, 
                                          subst("- The &1 in position &2 was converted to &3.",
                                                 CONVERTABLE_DESCRIPTIONS_ARRAY[iConvertNum],
                                                 i - iSkippedBytes,
                                                 CONVERT_DESCRIPTIONS_ARRAY[iConvertNum]
                                                 )     
                                           ,"~n").
                                           
                    iWarningCount = iWarningCount + 1. 
                end.    
                else  
                    undo, throw new UnprocessableEntityError(subst("Unconvertable character (ascii &1) found in position &2 in &3 &4 cannot be converted from codepage UTF-8 to &5.~n"
                                                                  + "Please replace the unconvertable character with a character using the keyboard. If this is a common occcurance contact IT and request auto conversion of ascii (&1) to a supported character." ,
                                                                  iByte,
                                                                  i - iSkippedBytes,
                                                                  pcTableName,
                                                                  pcPropertyLabel,
                                                               //   BufferUtil:GetRowName(phfield:buffer-handle,true),
                                                               //   BufferUtil:GetFieldLabel(phfield),
                                                                  session:cpinternal
                                                               )
                                                       ).
               
              
            end.
            else
                cConverted = cconverted + chr(iByte).
                
        end.
        if cWarning > "" then  
            pcWarning = subst("&1~n&2",
                             subst("The &1 &2 was saved with &3 that could not be saved as-is in the internal &4 code page:",
                                   pcTableName,
                                   pcPropertyLabel,
                                   if iWarningCount > 1 then subst("changes to &1 characters",iWarningcount) else "one change to a character", 
                                   session:cpinternal),
                              cWarning     
                            ).
                            
        return cConverted.    
    end method. 
    
end class.
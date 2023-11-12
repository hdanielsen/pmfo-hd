 
 /*------------------------------------------------------------------------
    File        : AngularModelGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 24 10:29:38 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.StringUtil from propath.
using OpenEdge.Core.StringConstant from propath.

block-level on error undo, throw.

class Pmfo.Tools.AppBuilder.AngularModelGenerator: 
    define stream codegen. 
    
    define temp-table ttField 
        field handle as handle
        field sortname as character
        field sortseq as integer .
    
    define public property OutputDir as character no-undo get. set.
    
    method public character Generate(pcFolder as character,phBuffer as handle):
        return Generate(pcfolder,phBuffer, "", false).
    end method.
    
    method protected character NumberedSearchable(pcsearchable as character):
        define variable iInt as integer no-undo.
        define variable cSearchable as character no-undo.
        define variable i as integer no-undo.
        define variable cDlm as character no-undo.
      
        do on error undo, throw:
            // if there alreays us a nu,ber then keep as-os
            iInt = integer(entry(1,pcSearchable)).
            // no error means there already is a nunber so just return as-is
            return  pcsearchable. 
            catch e as Progress.Lang.Error :
               // continue processing below 
            end catch.
        end.
        do i = 1 to num-entries(pcsearchable):
            assign
                cSearchable = cSearchable
                            + cDlm 
                            + substitute("&1,&2",i,entry(i,pcsearchable))
                cDlm = ","            
                .                            
        end.
        return cSearchable.
    end.    
    
    method public character Generate(pcfolder as character, phBuffer as handle, pSearchable as character, plFormBuilder as logical):
        define variable iFLd as integer no-undo.
        define variable hFld as handle no-undo.
        define variable cModel as character no-undo.
        define variable iAt as integer no-undo.
        define variable cName as character no-undo.
        define variable cComment as character no-undo.
        define variable iSearch as integer no-undo.
        define variable iSearchNum as integer no-undo.
        define variable iFldSeq as integer no-undo.
        define variable cBaseString as character no-undo.
        define variable cFile as character no-undo.
        pcFolder = replace(pcFolder,StringConstant:BACKSLASH,"/"). 
        pSearchable = NumberedSearchable(pSearchable).
        cModel = BufferUtil:SingularizeSerializeName(phBuffer:serialize-name,true).
        cFile =  subst("&3/&2&1.model.ts",
                       lc(cModel),
                       if Outputdir > "" then left-trim(Outputdir,"/") + "/" else "",
                       pcFolder
                       ).
        output stream codegen to value(cFile).
        
        cBaseString = "BaseModel".
        
        if pSearchable > "" then
        do iFLd = 2 to num-entries(pSearchable) by 2
        on error undo, throw:
           phbuffer:buffer-field(entry(iFld,pSearchable)).
           if index(cBaseString,"Searchable") = 0 then
                cBaseString = cBaseString + ", Searchable".
           catch e as Progress.Lang.Error :
              undo, throw new IllegalArgumentError(subst("Searchable Field '&1' does not exist in '&2'",entry(iFld,pSearchable),phBuffer:name)).     
           end catch.  
        end.
        
        empty temp-table ttField.
        do iFld = 1 to phbuffer:num-fields:
            hfld = phbuffer:buffer-field(iFld). 
            if hFLd:serialize-hidden = false then 
            do:
                
               if hFLd:name = "plant-id" then 
                   cBaseString = cBaseString + ", VaryByPlant".
               if index(hFLd:serialize-name,"@") > 0  and index(cBaseString,"Alias") = 0 then
               do:
                   cBaseString = cBaseString + ", Alias". 
               end.        
               create ttField.
               iFldSeq = 0.
               iFldSeq = int(substr(hFld:serialize-name,length(hFld:serialize-name) - 1)) no-error.
               if iFLdSeq > 0 then 
               do:
                   ttField.sortname = substr(hFld:serialize-name,1,length(hFld:serialize-name) - 2).
                   ttField.sortseq = ifldSeq.
                  
               end.    
               else do: 
                  iFldSeq = int(substr(hFld:serialize-name,length(hFld:serialize-name))) no-error.
                  if iFLdSeq > 0 then 
                  do:
                      ttField.sortname = substr(hFld:serialize-name,1,length(hFld:serialize-name) - 1).
                      ttField.sortseq = ifldSeq.
                
                  end.
                  else
                     ttField.sortname = hFld:serialize-name.
                    
               end.
               ttField.handle = hFld.
            end.
        end.    
        if plFormBuilder then                  
            put stream codegen unformatted 
                "import ~{ FormBuilder, FormControl, FormGroup ~} from '@angular/forms';" skip.
        
        put stream codegen unformatted 
           subst("import ~{ &1 ~} from './base.model';",cBaseString) skip.
                
        put stream codegen unformatted skip(1)
             subst("export class &1 extends BaseModel ~{",cModel) skip 
             subst("static readonly typeName = '&1';",StringUtil:LowerCaseFirst(cModel)) at 3 skip. 
        
        
        for each ttField by sortname by ttField.sortseq:
            hfld = ttField.handle. 
            if hFLd:serialize-hidden = false then 
            do: 
                cName = hfld:serialize-name.
                if hfld:name = "plant-id" then
                do:
                    put stream codegen skip(1).
                    put stream codegen unformatted '@VaryByPlant() ' at 3 cname. 
                end.  
                else do:
                    if pSearchable > "" then do: 
                        iSearch = lookup(hFld:name,pSearchable).
                        if iSearch > 0 then 
                        do: 
                            iSearchNum = (iSearch / 2) - 1.
                            put stream codegen skip(1). 
                            put stream codegen unformatted subst("@Searchable('&1', '&2', &3)",
                                                                BufferUtil:GetFieldLabel(hFld),
                                                                GetDataType(hFld:data-type,output cComment),
                                                                iSearchNum) at 3.
                            
                        end. 
                    end.   
                    iAt = index(cName,"@").
                    if iAt > 0 then
                    do: 
                        // keep known inconsisttency for now 
                       if phBuffer:name = "ttPartner" and hFld:name = "AcctIdName" then
                          cName = substr(cName,1,iAt - 1) 
                                + caps(substr(cName,iAt + 1,1)) 
                                + substr(cName,iAt + 2). 
                       else 
                          cName = BufferUtil:SingularizeSerializeName(substr(cName,1,iAt - 1)) 
                                + caps(substr(cName,iAt + 1,1)) 
                                + substr(cName,iAt + 2). 
                      
                       put stream codegen skip(1). 
                       put stream codegen unformatted
                           subst("@Alias('&1') ",hfld:serialize-name) at 3  cname.
                    end.   
                    else
                       put stream codegen unformatted cName at 3.
                    
                end.
                put stream codegen unformatted ": " GetDataType(hFld:data-type,output cComment) ";". 
                if cComment > "" then
                   put stream codegen unformatted "// " at 50 cComment .   
               
            end.
        end.
        if plFormBuilder then
        put stream codegen unformatted skip(1)
             "createFormBuilder(fb: FormBuilder)~{" at 3
               "let formGroup = new FormGroup({});"   at 5 
               "for (let prop in this) ~{"            at 5
                 "formGroup.addControl(prop, new FormControl(this[prop]));" at 7
               "~}" at 5
               "return formGroup;" at 5
             "~}" at 3.
        
        put stream codegen unformatted skip "~}".
        return cfile.
        finally:
            output stream codegen close.        
        end finally.
    end method.
    
    method protected character  GetDataType(pcDataType as character, output pcComment as char):
        case pcDataType :
            when "character" or when "clob" then
               return "string".
            when "integer" or when "int64" then
            do:
               pcComment = pcDataType.
               return "number".         
            end.
            when "decimal" then
               return "number". 
            when "logical" then
               return "boolean". 
            when "date" or when "datetime" or when "datetime-tz" then
               return "Date".
            when "blob" then
            do:
               pcComment = "base64 encode".
               return "string".  // "Blob" ???
            end.     
            otherwise 
               return lc(pcDataType).    
       end case.     
    end method.        
end class.
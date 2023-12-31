/**********************************************************************
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE. 
***********************************************************************/
/*------------------------------------------------------------------------
    File        : DatasetJsonWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Havard Danielsen, Progress Software Corporation
    Created     : 2020
    Notes       : 
----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Common.DatasetProcessor from propath.

// expression that adds escape (backslash) to values that JSON convention is expecting to be escaped with backslash
// Note that backslash is escapped first so as to not replace all the other backslashes.  
&scop jsonescape  replace(replace(replace(replace(replace(replace(replace(replace(~{&charvalue~},'~\','~\~\'),StringConstant:BACK,'~\b'),StringConstant:FF,'~\f'),StringConstant:LF,'~\n'),StringConstant:CR,'~\r'),StringConstant:TAB,'~\t'),'/','~\/'),'=','~\=')

class Pmfo.Core.Common.DatasetJsonWriter inherits DatasetProcessor: 
    define stream jsonfile.  
    
    define public property FileName         as character no-undo get. protected set. 
    define public property FullFileName     as character no-undo 
        get():
            file-info:file-name = FileName.
            return file-info:full-pathname.
        end.
        
    // set to rue to ommit dataset wrapper   
    define public property OmmitDataset     as logical   no-undo get. set. 
    
    // comma separated list of buffer names to be serialized as object and not array  
    define public property SingularBuffers  as character no-undo get. set. 
    
    /* Properties that track state across method calls */ 
    define protected property AddComma      as logical   no-undo get. set. 
    define protected property Level         as integer   no-undo get. set. 
 
  // not used for performance reaons - it would be called a LOT  
/*    define protected property LinePos       as integer   no-undo*/
/*        get():                                                  */
/*            return Level * 2.                                   */
/*        end.                                                    */
          
    constructor public DatasetJsonWriter ():
        super ().
    end constructor.
 
    method protected void AddObjectHeader( pcName as character ):
        if AddComma then 
            put stream JsonFile unformatted ",".
        put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) quoter(pcName)  ": " StringConstant:CURLY_OPEN .
        Level = Level + 1.  
        AddComma = false. 
    end method.
    
    method protected void AddObjectHeader( ):
        if AddComma then 
            put stream JsonFile unformatted ",".
        put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) StringConstant:CURLY_OPEN .
        Level = Level + 1.  
        AddComma = false.    
    end method.
    
    method protected void AddObjectFooter(  ):
        Level = Level - 1.
        put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) StringConstant:CURLY_CLOSE. 
    end method.
    
    method protected void AddArrayHeader( pcName as character ):
        if AddComma then 
            put stream JsonFile unformatted ",".
        put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) quoter(pcName) ": " "[".
        Level = Level + 1.  
        AddComma = false. 
    end method.
       
    method protected void AddArrayFooter(  ):
        Level = Level - 1.  
        put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) "]"  .  
    end method.
    
    method public void Write( input phDataset as handle,input pcFilename as character ):
         FileName = pcFileName.
         ProcessDataset(phDataset).
    end method.
     
    method override protected void ProcessDataset( input phDataset as handle ):
        output stream jsonfile to value(Filename) convert source session:charset target "UTF-8" no-map no-echo.
        if OmmitDataset then
            AddObjectHeader().
        else do:
            // add the dataseth header  - this is done here to mimic the write-json formatting (not a json requirement)
            // (cannot use AddObjectHeader since it is not consistent with the rest of the json logic) 
            put stream JsonFile unformatted StringConstant:CURLY_OPEN quoter(phDataset:serialize-name) + ": " + StringConstant:CURLY_OPEN.
            Level = 1.
        end.
        super:ProcessDataset(input phDataset).
        AddObjectFooter().
        
        // the dataset end node is added on the same line as whatever node was written
        // to be consistent with write-json (not a json requirement)
        if OmmitDataset = false then
            put stream JsonFile unformatted StringConstant:CURLY_CLOSE. 
        
        finally:
           output stream jsonfile close.         
        end finally.
              
    end method.

    method override protected void ProcessBuffer( input phBuffer as handle ):
        define variable lArray as logical no-undo.
        lArray = SingularBuffers = "" or lookup(phbuffer:name,SingularBuffers) = 0.
        if lArray then
            AddArrayHeader(phBuffer:serialize-name).
            
        super:ProcessBuffer(input phBuffer).
        if lArray then
            AddArrayFooter().     
    end method.
    
    method override protected void ProcessRow( input phBuffer as handle ):
        define variable lArray as logical no-undo.
        lArray = SingularBuffers = "" or lookup(phbuffer:name,SingularBuffers) = 0.
        if lArray then
            AddObjectHeader().
        else      
            AddObjectHeader(phBuffer:serialize-name).
        ProcessFields(phBuffer).
        super:ProcessRow(input phBuffer).
        AddObjectFooter(). 
    end method.
    
    method protected override void ProcessEmptyRow(phBuffer as handle):
        if SingularBuffers > "" and lookup(phbuffer:name,SingularBuffers) > 0 then
        do:
            if AddComma then                                                                                                                                                          
                put stream JsonFile unformatted ",".  
            put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) quoter(phbuffer:serialize-name) ": " "null".
         //   AddComma = true.
        end.
        AddComma = true. 
    end method.
    
    // hide foreign fields return the fieldhandles that were hidden
    // This is written in 11.7 so the extent may have null values if fields already are hidden . 
    method protected handle extent HideForeignFields(phRelation as handle):
        define variable i as integer no-undo.
        define variable cField as character no-undo.
        define variable hField as handle no-undo.
        define variable hHidden as handle extent no-undo.
        define variable iCount as integer no-undo.
        extent(hHidden) = integer(num-entries(phRelation:relation-fields) / 2).
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cField = entry(i + 1,phRelation:relation-fields).
            hField = phRelation:child-buffer:buffer-field(cField).
            if hField:serialize-hidden = false then
            do: 
                iCount = iCount + 1.
                hHidden[iCount] = hField.
                hField:serialize-hidden = true.  
            end.    
        end.   
        return hHidden. 
    end method.
    
    method protected void ProcessFields( input phBuffer as handle ):
        define variable iFld           as integer   no-undo.
        define variable hFld           as handle    no-undo.
        define variable iExtent        as integer   no-undo.
        define variable iColStart      as integer   no-undo.
        define variable iColEnd        as integer   no-undo.
        define variable cLabel         as character no-undo.
        define variable hForeignFields as handle extent no-undo.
        
       
        
        if valid-handle(phBuffer:parent-relation) 
        and phBuffer:parent-relation:nested
        and phBuffer:parent-relation:foreign-key-hidden then
        do:
            hForeignFields = HideForeignFields(phBuffer:parent-relation).
        end.   
         
        do iFld = 1 to phBuffer:num-fields on error undo, throw:
            hFld = phbuffer:buffer-field(iFld).
            if hFld:serialize-hidden = false then 
            do: 
                if hfld:extent > 0 then
                do:
                    AddArrayHeader(hfld:serialize-name).
                    assign
                        cLabel = ""
                        iColStart = 1
                        iColEnd   = hfld:extent
                        .
                end.
                else
                do: 
                    // set iterator to 0 so we can use buffer-value(0) and thus utilize the same code for extent and non-extent variables 
                    assign
                        cLabel    = quoter(hFld:serialize-name) + ": " 
                        iColStart = 0
                        iColEnd   = 0
                        .
                end.        
               
                // Add comma to prev line if applicable - AddComma is false when ProcessFields is called and is set to true after the first field is written below
                if AddComma then                                                                                                                                                          
                    put stream JsonFile unformatted ",".   
                    
                do iExtent = iColStart to iColEnd.
                    // if we are writing extents add comma on previous line if this is not the first 
                    if iExtent > 1 then
                        put stream JsonFile unformatted ",".   
                    put stream JsonFile unformatted skip fill(StringConstant:Space,(Level * 2)) cLabel.
                    if hFld:buffer-value(iExtent) = ? then
                        put stream JsonFile unformatted "null".
                    else do:
                        case hFld:data-type:                        
                            when "character" then
                                put stream JsonFile unformatted
                                       // escape all the json convention stuff 
                                    &scop charvalue hFld:buffer-value(iExtent)
                                    quoter({&jsonescape})
                                    &undefine charvalue 
                                    .
                                
                            when "clob" or when "blob" then
                                CopyLobToFile(hFld).
                            when "integer" then
                                put stream JsonFile unformatted hFld:buffer-value(iExtent) .
                            when "decimal" then
                            do:
                                put stream JsonFile unformatted hFld:buffer-value(iExtent).
                                // if no decimals add .0 to be consistent with write-json 
                                if hFld:buffer-value(iExtent) = int(hFld:buffer-value(iExtent)) then
                                    put stream JsonFile unformatted ".0" .
                            end.
                            when "date" or when "datetime" or when "datetime-tz" then  // iso date   
                                put stream JsonFile unformatted quoter(iso-date(hFld:buffer-value(iExtent))).
                            when "logical" then
                                put stream JsonFile unformatted if hFld:buffer-value(iExtent) = true then "true" else "false"  .
                            
                        end.    
                    end.
                end.
                if hfld:extent > 0 then
                    AddArrayFooter().
                      // set to true for the next field in this method as well as the next row or next array 
                AddComma = true.    
            end.  
        end.
        
        // in the unlikely case that all fields are hidden...
        AddComma = true.    
       
        finally:
            do iFld = 1 to extent(hForeignFields):
                if valid-handle(hForeignFields[iFld]) then
                     hFld:serialize-hidden = false.
            end.    
        end.    
    end method.
    
    method protected void CopyLobToFile(phfld as handle):
        define variable cLong as longchar no-undo.
        define variable mMem  as memptr no-undo. 
        if phfld:data-type = "clob" then 
            copy-lob phFld:buffer-value to cLong.
        else if phfld:data-type = "blob" then
        do:    
            copy-lob phFld:buffer-value to mmem.
            cLong = base64-encode(mMem).
            set-size(mMem) = 0.
        end.    
        &scop charvalue cLong
        cLong = {&jsonescape}. // escape all the json convention stuff 
        &undefine charvalue
         // workaround 11.7.? limitation quoter does not support longchar 
        put stream JsonFile unformatted '"'.
        output stream jsonfile close.  
        copy-lob cLong to file FullFileName append convert target codepage "UTF-8"    .
        output stream jsonfile to value(Filename) convert source session:charset target "UTF-8" no-map no-echo append.
        // workaround 11.7.? limitation quoter does not support longchar 
        put stream JsonFile unformatted '"'.
        
    end method.
    
end class.
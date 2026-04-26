 
 /*------------------------------------------------------------------------
    File        : MemoryLeakUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Sep 18 06:31:13 EDT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
 
using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.MemoryObjectBE from propath.
 
 

block-level on error undo, throw.

class Pmfo.Util.MemoryLeakUtil: 
     
    
   define static temp-table ttMemoryObject no-undo serialize-name "memoryObjects" {1}
   field name                             as character   serialize-name "name"
   field publicName                       as character   serialize-name "publicName"
   field publicParentName                 as character   serialize-name "publicParentName"
   field handle                           as handle      serialize-name "handle"   
   field parentHandle                     as handle      serialize-name "parentHandle"   
   field createdBy                        as character   serialize-name "createdBy"   
   field info                             as character   serialize-name "info" 
   field problem                          as character   serialize-name "problem" 
   field type                             as character   serialize-name "type"
   field sessionId                        as integer     serialize-name "sessionId"
   field zz_seq                           as int64       serialize-hidden
   index idx as unique name handle
   index zz_seq as primary zz_seq
   .
   
    method public static void TestMemory(pcMessage as char ):
        define variable oBE as MemoryObjectBE no-undo.
        define variable hDataset as handle no-undo.
        define variable hTable as handle no-undo.
        obe = new  MemoryObjectBE().
      //  oBE = cast(this-object:ServiceManager:CreateBusinessEntity("MemoryObjects"),MemoryObjectBE).
        
        oBe:GetData(output dataset-handle hDataset by-reference).
        
        hTable = hDataset:get-buffer-handle(1):table-handle.
          message pcmessage
        view-as alert-box.
        displayMEm(pcmessage ,input table-handle hTable by-reference).
          
    end method.    
    
    method private static void displayMEm(pcmessage as char, input table ttMemoryObject):
        current-window:width = 400.
        current-window:title = pcMessage.
        for each ttmemoryObject with width 400:
         
            disp ttmemoryObject.name format "x(25)" 
                 ttmemoryObject.publicname format "x(25)"
                 ttMemoryObject.info format "x(25)" 
                 int(ttMemoryObject.parentHandle) 
                 integer(ttmemoryObject.handle) 
                 
                 ttmemoryObject.type.
        end.           
    end method.
   
end class.
 
 /*------------------------------------------------------------------------
    File        : DebugUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 22 06:50:21 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

block-level on error undo, throw.

class Pmfo.Util.DebugUtil: 
    method public static void ShowDataset(phDataset as handle, pcMessage as character):
        define variable i as integer no-undo.
        define variable cc as character no-undo.
        define variable hBuffer as handle no-undo.
        define variable cTitle as character no-undo.
        define frame xx 
          with down width 200.
        
        frame xx:title = subst("Dataset Handle: &1 Name: &2 - &3",phDataset,phDataset:name,pcMessage).
        
        current-window:width = 300.
        do i = 1 to phDataset:num-buffers with frame xx    :
            hBuffer = phDataset:get-buffer-handle(i). 
            display 
                hBuffer:name format "x(32)" label "Buffer Name" 
                hBuffer:serialize-name format "x(32)" label "Buffer Serialize Name"
                string(hBuffer:handle) format "x(10)" label "Buffer Handle" 
                
                hBuffer:table-handle:name format "x(32)" label "Table Name" 
                hBuffer:table-handle:serialize-name format "x(32)" label "Table Serialize Name"
                string(hBuffer:table-handle) format "x(10)" label "Table Handle" 
                 
               .
            down.
        end.    
    end method.  
end class.
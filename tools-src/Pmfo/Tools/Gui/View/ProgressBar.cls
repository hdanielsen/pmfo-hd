 
 /*------------------------------------------------------------------------
    File        : ProgressBar
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 01 08:28:55 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.View.Component.
using Pmfo.Tools.Util.ColorManager from propath.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.
using Pmfo.Tools.Gui.Model.CancelError from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.View.ProgressBar inherits Component implements IMonitorTarget: 
     
    define private rectangle rBottom 
        edge-pixels 0  
        size 1 by 1
        bgcolor 8. 
     
    define private rectangle rTop
        edge-pixels 0
        size 1 by 1
       .
     
    define private variable fiText as character format "X(256)":U   
        view-as text 
        size 20 by 0.7 no-undo
       
     .
     define private variable fiPct as integer format ">>Z%":U   
        view-as text 
        size 5 by 0.7 no-undo
       
     .
     
     define frame Progress-frame
        fiText  at col 3 row 1 no-label
        rBottom at col 2 row 1.8 
        rTop    at col 2 row 1.8
        fiPct   at col 20 row 1 no-label
        with
        three-d no-box side-label 1 down  
      no-underline overlay.  
      
    define public property Canceled as logical no-undo get. set.
    define public property Text as character no-undo 
        get. 
        set(pcText as char):
            fiText:screen-value in frame Progress-frame = pctext.
            this-object:Text = pcText.
        end.
    define public property Total as int64 no-undo get. set.
    define public property Count as int64 no-undo get. set.
    
    constructor public ProgressBar (  input pParent as handle ):
        super (input frame Progress-frame:handle, input pParent).
        Resize(pParent).
        enable all with frame Progress-frame.
        rTop:hidden in frame Progress-frame = true.
        rTop:bgcolor = ColorManager:LimeGreen.
        this-object:Disable().
    end constructor.
    
    method public void Increment():
       Increment(1). 
    end.      
    
    method public void Complete():
        Increment(this-object:Total - this-object:Count).
    end method.    
    
    method public void Increment(pCount as int64):
        define variable iFullsize as integer no-undo.
        define variable dPct as decimal no-undo.
        if Canceled then 
            undo, throw new CancelError().
        Count = min(Count + pCount,Total).
        session:set-wait-state ("").
        process events.
        if Count > 0 and Total > 0 then
        do with frame Progress-frame:
            iFullSize = rBottom:width-p.
            dPct = count / total.
            fiText:screen-value = subst("&1 &2/&3",this-object:text,Count,Total).
            fiPct:screen-value = string(min(dPCt * 100,100)).
            rtop:width-p = min(max(1,dpct * iFullSize),iFullSize) no-error.
            rTop:hidden in frame Progress-frame = false no-error.
        end.
         
    end.      
    
    method public void Resize(phHandle as handle):
        do with frame Progress-frame:
            if phHandle:width < width then
            do:
                rBottom:width  = phHandle:width - 2 no-error.
                Increment(0).
                fiPct:col = phHandle:width - (fiPct:width).
                fiText:width = rBottom:width - 2 no-error.
                width = phhandle:width no-error.
            end.
            else do:
                width = phhandle:width no-error.
                rBottom:width  = phHandle:width - 2 no-error.
                Increment(0).
                 fiPct:col = phHandle:width - (fiPct:width).
                fiText:width = rBottom:width - 2 no-error.
               
            end.
        end.
           
    end method.    
    
end class.
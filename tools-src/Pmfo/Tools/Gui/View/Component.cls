 /*************************************************************/
  /* Copyright (c) 2011 by progress Software Corporation.      */
  /*                                                           */
  /* all rights reserved.  no part of this program or document */
  /* may be  reproduced in  any form  or by  any means without */
  /* permission in writing from progress Software Corporation. */
  /*************************************************************/
 /*------------------------------------------------------------------------
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :  
    Notes       : 
  ----------------------------------------------------------------------*/
routine-level on error undo, throw.
 
using Pmfo.Tools.Gui.View.IWidgetHandler from propath.
using Progress.Lang.AppError from propath.
using Pmfo.Tools.Gui.Model.Query from propath.
using Pmfo.Tools.Gui.View.IView from propath.
using OpenEdge.DataAdmin.Error.IllegalOperationError from propath.

class Pmfo.Tools.Gui.View.Component abstract implements IWidgetHandler, IView:
    
       /* hidden frame parent of browse - static browse cannot be added to frame */
    define private variable ViewFrame as handle no-undo.
    
    
    define public property Parent as handle no-undo 
        get():
            return ViewFrame:frame.
        end.       
        private set(h as handle): 
            ViewFrame:frame = h.
        end.
        
    define public property Row as decimal no-undo
        get():
            return ViewFrame:row.
        end.       
        set(drow as decimal): 
            ViewFrame:row = drow.
        end.
    
    define public property Col as decimal no-undo
        get():
            return ViewFrame:Col.
        end.       
        set(dcol as decimal): 
            ViewFrame:Col = dcol.
        end.
    
    define public property Width as decimal no-undo
        get():
            return ViewFrame:Width.
        end.       
        set(dwidth as decimal): 
            if dwidth > Width then 
            do:
                ViewFrame:Width = dwidth.
                 
            end.
            else do:  
               
                ViewFrame:Width = dwidth.
            end.
        end.
   	
   	 define public property Height as decimal no-undo
        get():
            return ViewFrame:Height.
        end.       
        set(dHeight as decimal): 
            
            if dHeight > Height then 
            do:
                ViewFrame:Height = dHeight.
              
            end.
            else do:  
                 
                ViewFrame:Height = dHeight.
            end.
       
        end.
   	
  
    define protected variable WidgetHandler as handle no-undo.
     
    /* expects browse with frame for parenting   */ 
    constructor public Component (hViewFrame as handle, pParent as handle):
        super ( ).   
        this-object:Create(hViewFrame,pParent).
    end constructor.
    
    /* allow uCreate to assign frame*/ 
    constructor public Component ():
        super ( ).   
    end constructor.
    
    method public void Init():    
    end method.
    
    method public void Create(hViewFrame as handle, pParent as handle):    
        if valid-handle(ViewFrame) then
            undo, throw new IllegalOperationError("Cannot call Create if frame is already defined for example if proviced in constructor"). 
        ViewFrame = hViewFrame.
        Parent = pParent. // setter assign as frame to viewframe 
        run Pmfo/Tools/Gui/View/eventhandler.p persistent set WidgetHandler (this-object).
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
 
     
    method public void SetFocus():
       // apply "entry" to browse.
    end method.
    
  
    method public void WidgetEvent(pcname as char):
        define variable hTable as handle no-undo.
        case pcname:
          
        end.               
    end method.
   
    method public void View(): 
        ViewFrame:hidden = false. 
    end method. 
    
    
    method public void Display(): 
        //Browse:hidden = true. 
    end method. 
    
    method public void Close(): 
        
    end method. 
    
    method public void Hide(): 
        ViewFrame:hidden = true. 
    end method. 
        
    method public void Disable(): 
        //Browse:sensitive = false. 
    end method. 
  
    method public void Enable(): 
       // Browse:sensitive = true. 
    end method. 
    
    destructor public Component ( ):
        delete object WidgetHandler no-error.
    end destructor.

end class.
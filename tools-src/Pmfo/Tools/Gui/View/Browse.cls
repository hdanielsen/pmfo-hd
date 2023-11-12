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
block-level on error undo, throw.
 
using Progress.Lang.AppError from propath.
using Pmfo.Tools.Gui.View.IView from propath.
using Pmfo.Tools.Gui.View.IWidgetHandler from propath.
using Pmfo.Tools.Util.ITheme from propath.

class Pmfo.Tools.Gui.View.Browse implements IWidgetHandler, IView:
    
    define public property SortExpression  as character no-undo get. protected set.
    define public property QueryExpression as character no-undo get. protected set.
    define public property Browse          as handle    no-undo get. protected set.
    define public property Theme           as ITheme    no-undo get. set.
    define public event DefaultAction signature void (table-handle hBuffer).
    define public event ValueChanged signature void (table-handle hBIuffer).
    
    define protected property Columns as handle extent get. set. 
    
       /* hidden frame parent of browse - static browse cannot be added to frame */
    define private variable BrowseFrame as handle no-undo.
    
    define public property Query as handle no-undo
        get():
            return Browse:query.
        end.       
        set(h as handle): 
            undo, throw new AppError("Setting of Query is currently not supported (causes crash...)",?).            
          //  Browse:query = h no-error.
        end.
    
    define public property Row as decimal no-undo
        get():
            return BrowseFrame:row.
        end.       
        set(drow as decimal): 
            BrowseFrame:row = drow.
        end.
    
    define public property Col as decimal no-undo
        get():
            return BrowseFrame:Col.
        end.       
        set(dcol as decimal): 
            BrowseFrame:Col = dcol.
        end.
    
    define public property Width as decimal no-undo
        get():
            return BrowseFrame:Width.
        end.       
        set(dwidth as decimal): 
            if dwidth > Width then 
            do:
                BrowseFrame:Width = dwidth.
                Browse:Width = dwidth.
            end.
            else do:  
                Browse:Width = dwidth.
                BrowseFrame:Width = dwidth.
            end.
        end.
   	
   	 define public property Height as decimal no-undo
        get():
            return BrowseFrame:Height.
        end.       
        set(dHeight as decimal): 
            
            if dHeight > Height then 
            do:
                BrowseFrame:Height = dHeight.
                Browse:Height = dHeight.
            end.
            else do:  
                Browse:Height = dHeight.
                BrowseFrame:Height = dHeight.
            end.
       
        end.
   	
   	define public property Down as integer no-undo get. protected set.
/*        get():                                      */
/*            return BrowseFrame:Down.                */
/*        end.                                        */
/*        set(idown as int):                          */
/*            message Browse:height browseframe:height*/
/*            view-as alert-box.                      */
/*            BrowseFrame:hidden = true.              */
/*            Browse:hidden =true.                    */
/*            BrowseFrame:scrollable = false.         */
/*            BrowseFrame:Height = session:height.    */
/*            Browse:Down = iDown.                    */
/*            message browse:height                   */
/*            view-as alert-box.                      */
/*            BrowseFrame:hidden = false.             */
/*            Browse:hidden =false.                   */
/*            message browse:height                   */
/*            view-as alert-box.                      */
/*            BrowseFrame:hidden = true.              */
/*            BrowseFrame:Height = Browse:height  .   */
/*            message BrowseFrame:Height browse:height*/
/*            view-as alert-box.                      */
/*                                                    */
/*            BrowseFrame:hidden = false.             */
/*                                                    */
/*                                                    */
/*            BrowseFrame:bgcolor=12.                 */
/*        end.                                        */
/*   	                                                    */
	define public property Parent as handle no-undo 
    	get():
            return BrowseFrame:frame.
    	end.       
    	set(h as handle): 
    	     BrowseFrame:frame = h.
        end.
    
    define protected variable WidgetHandler as handle no-undo.
     
    /* expects browse with frame for parenting   */ 
    constructor public Browse (hBrowse as handle, pParent as handle):
        define variable i as integer no-undo.
        super ( ).   
        Browse = hBrowse.
       
        BrowseFrame = Browse:frame.
        Parent = pParent.
        run Pmfo/Tools/Gui/View/eventhandler.p persistent set WidgetHandler (this-object).
       
        // needed for rowDisplay  - cannot access browse attributes like num-columns or first-column
        extent(this-object:Columns) =  this-object:Browse:num-columns.
        do i = 1 to extent(this-object:Columns):
            this-object:Columns[i] = this-object:Browse:get-browse-column(i).
        end.
    end constructor.
    
    method public void Init():    
        Browse:allow-column-searching = true.
        Browse:column-resizable = true.     
        on start-search of browse persistent 
           run widgetEvent in WidgetHandler ("start-search").   
        on default-action of browse persistent 
           run widgetEvent in WidgetHandler ("default-action").    
        on value-changed of browse persistent 
           run widgetEvent in WidgetHandler ("value-changed").    
        on row-display of browse persistent 
           run widgetEvent in WidgetHandler ("row-display").
        
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public void OpenQuery(  ):
        OpenQuery(?).
        return.
    end method.
    
    method public void OpenQuery(pcSort as char ):
        OpenQuery(?, pcSort).
        return.
    end method.
    
    method public void OpenQuery( pcExpression as char, pcsort as char ):
        define variable hBuffer as handle no-undo.
        define variable lDesc   as logical no-undo.
        define variable rRowid  as rowid no-undo.
        define variable hTable  as handle no-undo. 
        define variable cExpresion  as character no-undo.
        hBuffer = Browse:query:get-buffer-handle (1).
        if pcSort > "" then do:
            rRowid = hBuffer:rowid.
            if pcSort > "" then do:
                if SortExpression + " " begins pcSort + " " then 
                do:
                    lDesc = num-entries(SortEXpression,' ':U) > 1.
                    if not lDesc  then 
                        pcSort = pcSort + " " + "descending".
                end.
            end.     
            SortExpression = pcsort.
        end.    
        else if pcSort = "" then
            SortExpression = "". 
        
        if pcExpression = "" then 
            QueryExpression = "". 
        else if pcExpression > "" then 
            QueryExpression = pcExpression.
        
        define variable cExp as character no-undo.
        cExp = "for each " + hBuffer:name 
                                   + (if QueryExpression > "" then " where " + QueryExpression else "")  
                                   + (if SortExpression > "" then " by " + SortExpression else "").
/*         message cExp      */
/*         view-as alert-box.*/
        /***
         the desperate use of no-error below is to  avoid warning about briwse 
         notfitting in frame  when this fires after resort and columns have been
         made wide by column resize. It is possilbe that some of them are 
         unecessary,  but the error can occur in several places. It also seems(?) 
         as supressing it in one place may make it more likely to appear later  
        */ 
        Browse:query:query-prepare(cExp) no-error.
        Browse:query:query-open () no-error.
        ShowSort(pcSort).
        if rRowid <> ? then do:
           Query:reposition-to-rowid (rRowid) no-error.
        end.   
        else do:
         Query:get-first() no-error.
        end.
/*        ?if hBuffer:avail then do:*/
            hTable =  hBuffer:table-handle.
            ValueChanged:Publish(table-handle hTable by-reference).
/*        end.*/
        return.
    end method.
    
    method protected void FireValueChanged():
        define variable hTable as handle no-undo.
        hTable =  Query:get-buffer-handle(1):table-handle.
        ValueChanged:Publish(table-handle hTable by-reference).
    end method.
    
    // to be called from RowDisplay if row needs non-default color 
    method protected void SetRowColor(pFgColor as int, pBgColor as int):
        define variable iColumn as integer no-undo.
        do iColumn = 1 to extent(this-object:columns):
            this-object:columns[iColumn]:fgcolor = pFgColor.
            this-object:columns[iColumn]:bgcolor = pBgColor.
        end.    
    end.     
    
    method public void SetFocus():
        apply "entry" to browse.
    end method.
    
    method public void MoveAfter(h as handle):
        Browse:move-after-tab-item (h).
    end method.
    
    method public void MoveBefore(h as handle):
        Browse:move-before-tab-item (h).
    end method.
         
    method public void WidgetEvent(pcname as char):
        define variable hTable as handle no-undo.
        case pcname:
            when "start-search" then 
            do:
               Sort(). 
            end.  
            when "default-action" then do:
               hTable =  Query:get-buffer-handle (1):table-handle.
               DefaultAction:Publish(table-handle hTable by-reference).
            end.
            when "value-changed" then do:
               hTable =  Query:get-buffer-handle (1):table-handle.
               ValueChanged:Publish(table-handle hTable by-reference).
            end.
            when "row-display" then do:
                RowDisplay().
            end.    
        end.               
    end method.
    
    method protected void Sort():
        Browse:select-focused-row() no-error.
        Browse:set-repositioned-row (int(Browse:down / 2),"conditional").
        OpenQuery(Browse:current-column:name).
    end method.    
       
    method protected void RowDisplay():
    end method.          
    
    method public void View(): 
        Browse:hidden = false. 
        Browse:select-focused-row() no-error.
    end method. 
    
    method public void ShowSort(pcSort as char): 
        define variable lDesc as logical no-undo.
        define variable cColumn as character no-undo.
        define variable hColumn as handle no-undo.
        define variable iColumn as integer no-undo.
        Browse:clear-sort-arrows().
        if pcSort > '' then
        do:
            
           lDesc = num-entries(pcSort,' ':U) > 1.
           do iColumn = 1 to Browse:num-columns:
              assign
                  cColumn = ''
                  hColumn = Browse:get-browse-column(iColumn) 
                  cColumn = hColumn:name. 
              if cColumn = entry(1,pcSort,' ':U) then
              do:
                  hColumn:sort-ascending = not lDesc.
                  return.
              end. /* column = sortcolumn */
          end. /* column loop */
        end. /* SortColumn > '' */
        finally:
           apply "end-search" to browse.       		
        end finally.
    end method. 
    
    method public void Display(): 
         Browse:refresh(). 
    end method. 
    
    method public void Close(): 
        Browse:Query:query-close(). 
    end method. 
    
    method public void Hide(): 
        Browse:hidden = true. 
    end method. 
        
    method public void Disable(): 
        Browse:sensitive = false. 
    end method. 
  
    method public void Enable(): 
        Browse:sensitive = true. 
    end method. 
    
    destructor public Browse ( ):
        delete object WidgetHandler no-error.
    end destructor.

end class.
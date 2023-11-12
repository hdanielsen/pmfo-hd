 
 /*------------------------------------------------------------------------
    File        : FileBrowse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 15 17:48:48 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.View.Browse from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.View.FileBrowse inherits Browse: 
    {Pmfo/Util/filedata.i reference-only}
    define query qFile for FileData scrolling.

    define browse bFile query qFile
        display FileData.fullName format "x(32)"
                FileData.fileType format "X"
        WITH NO-ROW-MARKERS SEPARATORS  
          
       10 DOWN    .
         
    define frame Browse-Frame
      bFile    
      with
      three-d no-box
      no-underline scrollable  overlay.  
      /* title "Tenants" set from Presenter:title */
    
    constructor public FileBrowse (hParent as handle ):
        super (bFile:handle in frame Browse-Frame, hParent ).     
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public void Bind(table fileData bind):
        message "bind" temp-table filedata:handle
        view-as alert-box.
    end method.
 
end class.
/*------------------------------------------------------------------------
    File        : IView
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 2011
    Notes       : 
--------------------------------------------------------------------------*/
interface Pmfo.Tools.Gui.View.IView: 
    
    define public property Row    as decimal no-undo get. set.
    define public property Col    as decimal no-undo get. set.
    define public property Width  as decimal no-undo get. set.
    define public property Height as decimal no-undo get. set.
    define public property Parent as handle  no-undo get.  
    
    method public void View(  ).
    method public void Hide(  ).
    method public void Enable(  ).
    method public void Disable(  ).
    method public void Display(  ).    
    method public void Init(  ).
    method public void Close():
    method public void SetFocus():
      
end interface.
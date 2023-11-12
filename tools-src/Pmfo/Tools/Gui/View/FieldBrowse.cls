 
 /*------------------------------------------------------------------------
    File        : FileBrowse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 15 17:48:48 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Tools.Gui.View.Browse from propath.

class Pmfo.Tools.Gui.View.FieldBrowse inherits Browse: 
    {Pmfo/Tools/AppBuilder/fieldData.i reference-only}
   
    define public event FieldValueChanged signature void (table FieldData).
    define public property UnresolvedSerializeName as char no-undo get. set.  
        
    define private query qField for fieldData scrolling.
    define private browse bField query qField
        display FieldData.isExcluded    column-label "Exclude" view-as toggle-box
                FieldData.serializeName format "x(50)" width 29.15
                FieldData.FieldName     format "x(50)" width 29.15
                FieldData.dataType      format "x(9)" 
                FieldData.inSDO         column-label "In SDO" view-as toggle-box 
                FieldData.isCalc        column-label "Calc" view-as toggle-box 
        enable FieldData.isExcluded FieldData.serializeName        
        WITH NO-ROW-MARKERS SEPARATORS  
        27 DOWN
        title "Fields"    .
         
    define frame Browse-Frame
      bField    
      with
      three-d no-box
      no-underline scrollable  overlay.  
      
    constructor public FieldBrowse (hParent as handle ):
        super (bField:handle in frame Browse-Frame,hParent ).
    end constructor.
   
    method public override void Init():    
        on value-changed of fielddata.serializeName in browse bfield persistent 
           run widgetEvent in WidgetHandler ("field-value-changed").   
        on value-changed of fielddata.isExcluded in browse bfield persistent
           run widgetEvent in WidgetHandler ("field-value-changed").      
        
        super:Init().
    end method.
    
    method override public void WidgetEvent(pcname as char):
        define variable hTable as handle no-undo.
        case pcname:
            when "field-value-changed" then 
            do:
                assign browse bfield fielddata.serializename
                       browse bfield fielddata.isExcluded.
                
                FieldValueChanged:publish(table FieldData by-reference).
                browse bField:refresh().
            end. 
            otherwise super:WidgetEvent(pcName). 
        end case.
    end method.
    
    method override protected void RowDisplay(  ):
        if valid-object(Theme) then 
        do:
            if fieldData.isExcluded then
                setRowColor(Theme:DisabledRowFg,Theme:DisabledRowBg).
            if fieldData.serializeName = UnresolvedSerializeName then 
                assign
                    fielddata.serializename:fgcolor in browse bField = Theme:ErrorFg
                    fielddata.serializename:bgcolor in browse bField = Theme:ErrorBg
                    .
        end.           
        super:RowDisplay().

    end method.
   
    method public void DisableIsExcluded():
        fielddata.isExcluded:read-only in browse bField = true.
    end.    
    
    method public void EnableIsExcluded():
        fielddata.isExcluded:read-only in browse bField = false.
    end.    
    
           
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public void Bind(table FieldData bind):
    end method.
 
end class.
 
 /*------------------------------------------------------------------------
    File        : SdoModel
    Purpose     : 
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : Fri Feb 22 10:06:30 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.Model.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using Pmfo.Tools.Gui.Model.AppBuilderModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
using Pmfo.Tools.Gui.Model.ISdoModel from propath.
using OpenEdge.DataAdmin.ITable from propath.
using OpenEdge.DataAdmin.IField from propath.
using OpenEdge.DataAdmin.IFieldSet from propath.
 

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.AbstractSdoModel abstract inherits AppBuilderModel implements ISdoModel : 
    define protected temp-table ttBuffer no-undo 
       field name as char
       field db  as char
       field tableName as char
       index name as primary unique name.
    
    define override public property ProcedureType as char no-undo init "SmartDataObject" get. 
    define public property DataLogicProcedure as character no-undo get. set.
    define public property DataBase as character no-undo get. protected set.
//    define property Table as character no-undo get. protected set.
    
    define public property QueryName     as character no-undo get. protected set.
    define public property BaseQuery     as character no-undo get. protected set.
    define public property AssignList    as character no-undo get. protected set.
    define public property DataFields    as character no-undo get. protected set.
    define public property EnabledFields as character no-undo get. protected set.
    define public property Fields        as ISet      no-undo get. protected set.      
    define protected property ReadSettings as logical no-undo get. set.     
    
    constructor public AbstractSdoModel (  ):
        super().
    end constructor.
    
    
    method public char extent GetBuffers():
        define variable cBuffers  as character extent no-undo.
        define variable iCnt as integer no-undo.
        for each ttbuffer:
            icnt = icnt + 1.
        end.
        if iCnt > 0 then do:
            extent(cBuffers) = icnt.
            iCnt = 0.
            for each ttbuffer:
                icnt = icnt + 1.
                cBuffers[icnt] = subst("define buffer &1 for &2.",ttBuffer.name,ttbuffer.tablename).
            end.
        end.
        return cBuffers.     
    end method.    
    
    method public void AddFieldsFromDataAdmin(pTable as ITable).
        define variable oDbIter     as OpenEdge.DataAdmin.Lang.Collections.IIterator  no-undo.
        define variable oIter       as OpenEdge.Core.Collections.IIterator  no-undo.
        define variable oNewField   as FieldModel  no-undo.
        define variable oField      as FieldModel  no-undo.
        define variable oDbField    as IField   no-undo.
        define variable odbFieldset as IFieldSet no-undo.
        define variable cAssignName as character no-undo.
        
        oDbfieldSet = pTable:Fields.
        oDBIter = oDbfieldSet:Iterator().
        dbloop:
        do while oDbIter:HasNext():
            oDbField = cast(oDbIter:Next(),IField).
            oNewField = new FieldModel(pTable:name, oDBField:name).
            if not this-object:Fields:Contains(oNewField) then 
            do: 
                oIter = this-object:Fields:Iterator().
                do while oIter:HasNext():
                    oField = cast(oIter:Next(),FieldModel).
                    // don't add the field if it is already here with a different  name
                    if oField:sourceTable = pTable:name and oField:sourceName = oDbField:name then 
                        next dbloop.
                end.
                
                oNewField:dataType = oDBField:datatype.
                this-object:fields:add(oNewField).
            end.
        end.      
            
    end method.
     
end class.
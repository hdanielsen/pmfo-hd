 
 /*------------------------------------------------------------------------
    File        : ParameterModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Mar 07 15:48:00 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.IOModeEnum from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.ParameterModel: 
    define public property Name     as char no-undo get. set.
    define public property IoMode   as IOModeEnum no-undo get. set.
    
    define protected property DataTypeEnum as DataTypeEnum no-undo 
    get.
    set.
    
    define public property DataType as character no-undo 
    get():
        if DataType = "" and valid-object(DataTypeEnum) then
            return lc(DataTypeEnum:ToString()).
        return DataType.     
    end.
    set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ParameterModel (pcName as character,poIoMode as IOModeEnum,poDataType as DataTypeEnum):
        super ().
        assign
            Name     = pcName
            IoMode   = poIoMode
            DataTypeEnum = poDataType
            .
        
    end constructor.
    
    constructor public ParameterModel (pcName as character,poIoMode as IOModeEnum,poDataType as character):
        super ().
        assign
            Name     = pcName
            IoMode   = poIoMode
            DataType = poDataType
            .
        
    end constructor.
    
    method public override character ToString():
        if DataType begins "dataset" or DataType begins "table" then
            return subst("&1 &2 &3",lc(IoMode:ToString()),DataType,name).
        else
            return subst("&1 &3 as &2",lc(IoMode:ToString()),DataType,name).
            
    end method.    
   
end class.
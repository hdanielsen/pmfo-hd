
/*------------------------------------------------------------------------
    File        : ISdoModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 24 09:10:13 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/
 
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.DataAdmin.ITable from propath.

interface Pmfo.Tools.Gui.Model.ISdoModel:  
    define public property Name as character no-undo get. 
    define public property ProcedureType as char no-undo  get. 
    define public property DataLogicProcedure as character no-undo get.
    define public property DataBase as character no-undo get. 
    define public property Table as character no-undo get. 
    
    define public property QueryName     as character no-undo get.
    define public property BaseQuery     as character no-undo get.
    define public property AssignList    as character no-undo get.
    define public property DataFields    as character no-undo get.
    define public property EnabledFields as character no-undo get.
    define public property QueryTables   as character no-undo get. 
    define public property Fields        as ISet      no-undo get.      
    
    method public void AddFieldsFromDataAdmin(pTable as ITable).
    
end interface.
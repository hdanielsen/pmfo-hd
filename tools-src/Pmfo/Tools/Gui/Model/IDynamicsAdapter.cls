
/*------------------------------------------------------------------------
    File        : IDynamicsAdapter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 24 12:20:22 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Tools.Gui.Model.IDynamicsAdapter:  
    method public void buildStructFromDB(
          input        pcDatasetCode as character, 
          input-output piRequestNo   as integer, 
          output       piNoTables    as integer). 
  
end interface.
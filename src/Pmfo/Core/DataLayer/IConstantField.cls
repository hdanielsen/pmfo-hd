
/*------------------------------------------------------------------------
    File        : IConstantField
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 09/10/2022
    Notes       : Define hard coded values for db fields that are not mapped to BE temp-table  
                  This is split from IHiddenSourceKey since it has hard coded value and can be used for 
                  the db primary key and in FindSource (for modify) 
  ----------------------------------------------------------------------*/   

using Pmfo.Core.DataLayer.ISourceField from propath.

interface Pmfo.Core.DataLayer.IConstantField inherits ISourceField:  
    define property QueryExpressionValue  as character no-undo get.
end interface.
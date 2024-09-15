
/*------------------------------------------------------------------------
    File        : IGetInternalDataDelegator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 03 08:42:05 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.BusinessLogic.Delegate.IDataDelegator.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.

interface Pmfo.Core.BusinessLogic.Delegate.IGetInternalDataDelegator inherits IDataDelegator:
   
    define public /** todo v12.7 final **/ property KEYS   as character no-undo get.
    // get internal data 
    method public logical GetData( input pQueryEntry as IQueryEntry).   
    
end interface.
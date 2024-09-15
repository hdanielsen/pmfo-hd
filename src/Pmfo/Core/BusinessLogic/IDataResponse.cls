
/*------------------------------------------------------------------------
    File        : IDataResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 2/18/2024
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Core.Collections.ICollection from propath.
 
using Pmfo.Core.Common.IGetDataMessages from propath.

interface Pmfo.Core.BusinessLogic.IDataResponse :  
    define public property Messages as IGetDataMessages  no-undo get. 
    
     
end interface.
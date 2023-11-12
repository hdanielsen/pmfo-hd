
/*------------------------------------------------------------------------
    File        : IHandleDataSourceMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 10 01:05:15 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.IDataSource from propath.
using OpenEdge.Core.Collections.IMap from propath.

interface Pmfo.Core.DataLayer.Util.IHandleDataSourceMap /* inherits IMap */ :  
    define public property Size as integer no-undo get.
    method public IDataSource Get(phBuffer as handle). 
    method public IDataSource Put(phBuffer as handle, pSource as IDataSource). 
    method public logical ContainsKey(phBuffer as handle). 
    method public IDataSource extent Sources(). 
    
end interface.

/*------------------------------------------------------------------------
    File        : IInfrastructureDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Sep 09 16:33:06 EDT 2021
                  Renamed Aug 26 2023    
    Notes       : renmaed 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.IList from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.

interface Pmfo.Core.DataLayer.IInfrastructureDataAccess inherits IDataAccess:  
    define public property Warnings as IList no-undo get. 
    method public void SubmitData( input phInfrastuctureDataset as handle, pApplicationDataset as handle). 
end interface.
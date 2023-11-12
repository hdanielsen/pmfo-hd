
/*------------------------------------------------------------------------
    File        : IServiceDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jun 05 16:32:07 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.IDataAccessResource from propath.

interface Pmfo.Core.DataLayer.IServiceDataAccess inherits IDataAccessResource:  
    define public property LegacyName as character no-undo get.
end interface.
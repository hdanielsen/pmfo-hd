
/*------------------------------------------------------------------------
    File        : IInfrastructureService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Aug 25 11:12:56 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.Common.IService from propath.

interface Pmfo.Core.Service.IInfrastructureService inherits IService:  
    define property Dataset as handle no-undo get. 
end interface.
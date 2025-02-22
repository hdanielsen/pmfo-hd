
/*------------------------------------------------------------------------
    File        : ISessionManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 16 15:19:55 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.Common.ISessionManager.

interface Pmfo.Core.Manager.ISessionManager inherits ISessionManager:  
    define public property QueryTimeout   as integer   no-undo get. set. 
    define public property IsDebug        as logical   no-undo get. set. 
    define public property ClientType     as character no-undo get. 
 
    method void Authorize(pcFormUrl as character).
     
end interface.
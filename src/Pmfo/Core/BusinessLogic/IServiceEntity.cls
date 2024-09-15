
/*------------------------------------------------------------------------
    File        : IServiceEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon May 27 08:42:17 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Property from propath.

interface Pmfo.Core.BusinessLogic.IServiceEntity inherits IEntity:  
    define public property Properties as Property extent no-undo get.  
    define public property Parameters as IParameter extent no-undo get.
    // Execute method for simple request with one segment in the url that returns data in properties 
    define public property Method     as Method no-undo get.  
    
    method public Method extent GetMethods() .
     
    // A GET for s ServiceEntity can return a set of properties without any method calls and execure is called without any reference in url
    // set this to true if a method name is required also for GET: servicename/methodname 
    define public property MethodOnly as logical no-undo get.  
    // Support passing parameters on url as an l;aterntive to the body
    // This was added to deal with cases implemented as GET by mistake and minimize changes needed on client to correct
    // It seems to be a slippery slope so as of current it need to be specifically allowed
    define public property AllowAction as logical no-undo get.  
    define public property WarningMessages as IWarningMessages no-undo get.
     
    // Reurn supported Web method - needed for entitites that support both POST and  GET 
    method public character MethodOperationType(pcMethodName as character).  
    // allow resolving public params from Method
    method public character GetPublicParameterName(pcName as character).
end interface.
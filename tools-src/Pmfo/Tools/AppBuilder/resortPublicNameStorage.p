
/*------------------------------------------------------------------------
    File        : refreshResources.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Thu May 16 08:01:07 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Web.SendExceptionError from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Repository.Business.PublicNameBE from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.Manager.StartupManager from propath.

define variable PublicNameBe as PublicNameBE   no-undo.


/* ************************  Function Prototypes ********************** */

function showError returns logical 
    (pError as Error,
     pTitle as char) forward.

/* ***************************  Main Block  *************************** */

if not valid-object(Application:ServiceManager) then 
    StartupManager:Instance. 
PublicNameBe = cast(cast(Application:ServiceManager,Pmfo.Core.Manager.IServiceManager):CreateBusinessEntity("publicNames"),PublicNameBe).
 
PublicNameBe:ResortStorage().
catch e as Progress.Lang.Error :
     showError(e,"Resort Storage Error").        
end catch.


/* ************************  Function Implementations ***************** */
function showError returns logical (pError as Error, pTitle as char):
    define variable lInner as logical no-undo.
    if type-of(pError,ApplicationError) then
    do:
        if valid-object(cast(pError,ApplicationError):InnerError) then
        do:
           pError = cast(pError,ApplicationError):InnerError.
           lInner = true.
        end.   
    end.
    else if type-of(pError,SendExceptionError) then
    do:
        if valid-object(cast(pError,SendExceptionError):InnerError) then
        do:
           pError = cast(pError,SendExceptionError):InnerError.
           lInner  = true.
        end.   
    end.
    message pError:GetMessage(1) skip
         pError:CallStack
       view-as alert-box error title pTitle. 
     
end function.

      

/*------------------------------------------------------------------------
    File        : IMethod
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 08:42:55 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.ISignature from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.

interface Pmfo.Core.Common.IMethod:  
     define public property Name             as character         no-undo get.
     // set for openAPi may also cause warnings  
     define public property IsDeprecated    as logical           no-undo  get. set.
     // optional info about how to repalce the call. i.e the issue that should be implemented to eliminate this usage 
     define public property DeprecatedRemoveCallInfo as character        no-undo  get. set.
     // optional warning text for deprecated method
     define public property DeprecatedWarning as character        no-undo  get. set.
 
     //  define public property Parameters       as IParameter extent no-undo get.  
 //    define public property Signatures       as ISignature extent no-undo get.  
     
 /* The current thinking is that we do not need parent in Functions and Actions are defined and handled in their main dataset
    If there are cases where we need to reach a function of a child temp-table then that 
    is the job of the *Router utilities (some code is already there, but may need improvements) 
    Update: The BusinessEntityOperationHandler now always execute the action in the request table's BE.
            But we should ideally be able to have nested methods though, and *Router classes may need improvement for that    
    */   
   //  define public property Parent           as character         no-undo get.  
  //   define public property Bound            as logical           no-undo get.
  // need some thought - can/should we enforce single row in body? 
   
     define public property IsPrimitive      as logical           no-undo  get. set.
       
     define public property SingleObject     as logical           no-undo get. set.
     define public property Binding          as MethodBindingEnum no-undo get. set.
        
     
     // what to call the type in error message 
     define public  property BaseTypeName  as character no-undo get.
     
     // should default to NONE - should only be defined for methods that 
     // need different (typically more) server types than the main requested buffer  
     define public  property ServerType    as ServerTypeEnum no-undo get.
     define public property Signatures     as ISignature extent no-undo get.  
     // return no content if successful 
     define public property NoContentResponse as logical no-undo get. set.
     
    // Tracked when findSignature is called succesfully to allow checks to get signature names
    // for example for cases that need to know the publicly exposed named parameters 
    // for cases where the data types are similar and different calls need to be handled 
    // by the same ABL method but we need to know which one   
     define public property CurrentParameterNames as character no-undo get. 
     
     method public ISignature DefineSignature(pParameter as IParameter).
     method public ISignature DefineSignature(pParameters as IParameter extent).
     method public ISignature DefineSignature().
   
     method public character GetPublicLinkTemplate(piSignature as integer).
     /** catalog info **/  
     method public JsonArray DescribeSignatures(pcPath as character, phBuffer as handle).
     method public logical   IsParametersRequired().
     // allow openapi to create parameters 
     method public void      InitializeSignatures().
     
     
end interface.
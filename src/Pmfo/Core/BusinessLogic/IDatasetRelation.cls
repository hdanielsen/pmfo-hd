
/*------------------------------------------------------------------------
    File        : IDatasetRelation
    Purpose     : Define a link/relation as conditional 
                  Active as a dataset relation on some methods only.
                  Defined as regular link    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 27 15:56:06 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IRelation.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.

interface Pmfo.Core.BusinessLogic.IDatasetRelation inherits IRelation:  
    /* The web request methods that should deactivate this relation */  
    define public property DeactivateMethods  as character extent no-undo get.
    // public for catalog
    define public property MethodNames        as character extent no-undo get.
    // deactivate requestst the specific method name 
    define public property MethodDeactivate   as logical no-undo get. set.
    define public property RelationHandle     as handle no-undo get. set.
    
    method public logical Deactivate(pRowState as RowStateEnum, pcServerMethod as character).
    
    /* 
     deactivate relation if the rowstate maps to one of the DeactivateMethods
     @return true if method is defined as a deactivateMethod 
    */  
    method public logical Deactivate(pRowState as RowStateEnum).
    // deactivate relation
    method public logical Deactivate().
    // activate relation
    method public logical Activate().
    
    method public void AddMethods(pMethods as IMethod extent).    
    method public void AddMethodName(pMethodname as character).
    
end interface.
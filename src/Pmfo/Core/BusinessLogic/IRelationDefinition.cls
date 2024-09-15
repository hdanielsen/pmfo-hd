
/*------------------------------------------------------------------------
    File        : IRelationDefinition
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 10:58:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.BusinessLogic.IQueryEntry.
using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.BusinessLogic.IRelationDefinition :
    /* the resource name of the entity that defines the relation */ 
    define public property ParentName     as character no-undo get.  
    /* the resource nameo on the other end */
    define public property PublicName     as character no-undo get.   
    define public property Cardinality     as RelationTypeEnum no-undo get. set.
    define public property XRef            as logical no-undo get. set.
    
end interface.
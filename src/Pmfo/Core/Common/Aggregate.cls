 
 /*------------------------------------------------------------------------
    File        : Aggregate
    Purpose     : Parsed odata aggregate entry 
    Syntax      : url query example to aggregate sum of property pieces serialized in response as totalPieees:
                   ?apply=aggregate(pieces with sum as totalPieces)
                  url query example to aggregate sum of property pieces by group serialized in response as totalPieees:
                  
                  childMults?apply=groupBy(invId(aggregate(pieces with sum as totalPieces))
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Dec 11 13:28:29 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Common.AggregationMethodEnum from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.Aggregate: 
    define public property SerializeName   as character get. protected set.
    define public property PublicFieldName as character get. protected set.
   // define public property FieldName       as character get. protected set.
    define public property AggregateMethod as AggregationMethodEnum get. protected set.
    define public property NavigationPath  as character get. protected set.
    
    constructor public Aggregate (pcPublicFieldName as character, poAggregateMethod as AggregationMethodEnum, pcSerializeName as character ):
        super ().
        if not valid-object(poAggregateMethod) and pcPublicFieldname <> "count" then  
            undo, throw new IllegalArgumentError("Constructor without aggregate only allowed for count function").
        
        PublicFieldName = pcPublicFieldName.
        AggregateMethod = poAggregateMethod.
        SerializeName = pcSerializeName.
    end constructor.
    
    method public override character toString():
        if not valid-object(AggregateMethod) and PublicFieldname <> "count" then  
            return subst("count as &1",PublicFieldName).
        else
            return subst("&1 with &2 as &3",PublicFieldName,lc(AggregateMethod:ToString()),SerializeName).
            
    end method.    
    
end class.
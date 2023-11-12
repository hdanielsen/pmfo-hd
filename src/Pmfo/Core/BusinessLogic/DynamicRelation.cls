 
 /*------------------------------------------------------------------------
    File        : DynamicRelation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 16 10:43:20 EDT 2022
    Notes       :  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IDynamicRelation from propath.
using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.Relation from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.DynamicRelation inherits Relation implements IDynamicRelation: 
    
      
    constructor public DynamicRelation (pParent as character, pChild as character ):
        super(pParent,pChild).
    end constructor.
    /**
    constructor public DynamicRelation (phBuffer as handle, pChild as character ):
        super(pParent,pChild).
    end constructor.
    **/
    method public override character PairsList().
        define variable oBE    as IBusinessEntity no-undo.
        define variable cPairs as character no-undo.
        define variable cKeys  as character no-undo.
        define variable cKeyArray as character extent no-undo.
        define variable hBuffer as handle no-undo.
       
        do on error undo, throw:
            return super:PairsList().
            catch e1 as IllegalStateError:
               // the super is a bit restrictive ..
               // if we get an error we'll try to fix the data and try again      
            end catch.
        end.
        
        oBe = this-object:ServiceManager:CreateBusinessEntity(ParentName).
        hBuffer = oBe:Datasets[1]:get-buffer-handle(1).
        cKeys = ServiceInfo:GetKeys(hBuffer).
        cKeyArray = Array:Split(cKeys).
        // default assumption is that the child has or will have  the same fields
        // since this is mainly used to create the child bufffer dynamically  
        CreateJoinPredicates(cKeyArray, cKeyArray).
        
        return super:PairsList().
        catch e2 as IllegalStateError:
            undo, throw e2.
        end.    
        catch e3 as Progress.Lang.Error :
            undo, throw new IllegalStateError(subst("Could not obtain relation information for parent '&1'",ParentName),e3).    
        end catch.  
    end method.
    
    // override for public assign    
    method public override IJoinPredicate extent CreateJoinPredicates(pParentField as character extent, pChildField as character extent):
        extent(JoinPredicates) = ?.
        JoinPredicates = super:CreateJoinPredicates(pParentField, pChildField).
        return JoinPredicates.
    end method.
    
   
end class.
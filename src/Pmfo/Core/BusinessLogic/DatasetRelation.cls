 
 /*------------------------------------------------------------------------
    File        : DatasetRelation
    Purpose     : Defines a relation on the dataset relation to allow the 
                  relation to be active on certain methods only and 
                  also behave as a regular link for catalog and json response 
                  building 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 27 15:32:52 EDT 2019
    Notes       : Everything is a bit backwards in the sense that deactivation
                  is the normal action state
                  For iMethod we default to activate of not specified in constructor,
                  but we do this by defining the logical property MethodDeactivate false  
                - Note that deactivateNMethods controls the relation when the parent 
                  is in the body and the publicName is NOT in the URL. If the
                  method is not disabled then the buffer will be added to the dataset.  
                - Set CanRead, CanUpdate or CanDelete false to 
                  control requests where the parent and child is the URL and 
                  the body or response only has the publicname entity    
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.DatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.Relation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.BusinessLogic.DatasetRelation inherits Relation implements IDatasetRelation: 
    define public property DeactivateMethods  as character extent no-undo get. protected set.
    //  action or function that is to activate or deactivate
    define public property MethodNames     as character extent no-undo get. protected set.
    // is the methodname supposed to deactive (true) or activate (false) the relation 
    define public property MethodDeactivate   as logical no-undo get. set.
    
    define protected property ValidMethods    as character no-undo init "GET,PUT,POST,DELETE" get. 
    
    define private property FieldPairs as character no-undo get. set. 
    
    define public property RelationHandle as handle no-undo 
        get. 
        set(pRel as handle): 
            RelationHandle = pRel.
            FieldPairs = RelationHandle:relation-fields.
        end.     
   
    constructor public DatasetRelation ( phRelation as handle, pCardinality as RelationTypeEnum  ):
        define variable cDeactivateDummy as character extent no-undo.
        this-object(phRelation,cDeactivateDummy,pCardinality).      
    end constructor. 
     
    constructor public DatasetRelation ( phRelation as handle, pMethod as IMethod ):
        //  default for action is to disable all regular crud requests
        this-object(phRelation,pMethod,Array:Split(ValidMethods)).      
    end constructor.
    
    constructor public DatasetRelation ( phRelation as handle, pMethods as IMethod extent):
        //  default for action is to disable all regular crud requests
        this-object(phRelation,pMethods,Array:Split(ValidMethods)).      
    end constructor.
    
    
    constructor public DatasetRelation ( phRelation as handle, pMethod as IMethod, pcDeactivateMethod as character ):
        this-object(phRelation,pMethod,Array:ToArray(pcDeactivateMethod)).      
    end constructor. 
    
    constructor public DatasetRelation ( phRelation as handle, pcDeactivateMethod as character ):
        this-object(phRelation,Array:ToArray(pcDeactivateMethod)).      
    end constructor.
    
    constructor public DatasetRelation ( phRelation as handle, pcDeactivateMethod as character, pCardinality as RelationTypeEnum ):
        this-object(phRelation,Array:ToArray(pcDeactivateMethod)).      
        Cardinality = pCardinality.
    end constructor.
    
    constructor public DatasetRelation ( phRelation as handle, pcDeactivateMethods as character extent):
        define variable oDummyMethod as IMethod no-undo.
        this-object(phRelation,oDummyMethod,pcDeactivateMethods).
    end method.
    
    constructor public DatasetRelation ( phRelation as handle, pcDeactivateMethods as character extent, pCardinality as RelationTypeEnum ):
        define variable oDummyMethod as IMethod no-undo.
        this-object(phRelation,oDummyMethod,pcDeactivateMethods).
        Cardinality = pCardinality.
    end method.
    
    constructor public DatasetRelation ( phRelation as handle, pMethod as IMethod, pcDeactivateMethods as character extent):
        this-object(phRelation,if valid-object(pMethod) then pMethod:Name else "", false, pcDeactivateMethods ).
    end constructor.     
    
    constructor public DatasetRelation ( phRelation as handle, pMethods as IMethod extent, pcDeactivateMethods as character extent):
        this-object(phRelation, GetMethodNames(pMethods), false, pcDeactivateMethods ).
    end constructor. 
    
    constructor public DatasetRelation ( phRelation as handle, pMethods as IMethod extent, pcDeactivateMethods as character extent, pCardinality as RelationTypeEnum):
        this-object(phRelation, GetMethodNames(pMethods), false, pcDeactivateMethods ).
        Cardinality = pCardinality.
    end constructor. 
    
    constructor public DatasetRelation ( phRelation as handle, pMethod as IMethod, plMethodDeactivation as logical, pcDeactivateMethods as character extent):
        this-object(phRelation,if valid-object(pMethod) then pMethod:Name else "",plMethodDeactivation, pcDeactivateMethods ).
    end constructor. 
    
    constructor public DatasetRelation ( phRelation as handle, pMethod as IMethod, plMethodDeactivation as logical):
       define variable cDummy as character extent no-undo.
        this-object(phRelation,if valid-object(pMethod) then pMethod:Name else "",plMethodDeactivation,cDummy).
    end constructor. 
    
    constructor public DatasetRelation ( phRelation as handle, pMethods as IMethod extent, plMethodDeactivation as logical):
       define variable cDummy as character extent no-undo.
        this-object(phRelation,GetMethodNames(pMethods),plMethodDeactivation,cDummy).
    end constructor. 
    
    constructor protected DatasetRelation ( phRelation as handle, pcMethodName as character, plMethodDeactivation as logical, pcDeactivateMethods as character extent):
        this-object(phRelation,GetSingleMethodNameAsArray(pcMethodName),plMethodDeactivation,pcDeactivateMethods).  
    end.
    
    constructor protected DatasetRelation ( phRelation as handle, pcMethodNames as character extent, plMethodDeactivation as logical, pcDeactivateMethods as character extent):
    
        define variable i as integer no-undo.
        
        super (phRelation:parent-buffer:serialize-name,phRelation:child-buffer:table-handle:serialize-name,CreateJoinPredicates(phRelation)).  
        
        if phRelation:child-buffer:table-handle:serialize-name <> phRelation:child-buffer:serialize-name then
            AliasName = phRelation:child-buffer:serialize-name.
        
        phRelation:relation-fields.
            
        Assert:NotNull(phRelation,"Relation").
        Assert:IsTrue(phRelation:type = "data-relation","Handle is data-relation").
        if plMethodDeactivation = false then
            Assert:HasDeterminateExtent(pcDeactivateMethods,"DeactivateMethods").
        RelationHandle = phRelation.    
         
        if (extent(pcMethodnames) = 1 and pcMethodnames[1] <> "") or extent(pcMethodnames) > 1 then
            MethodNames = pcMethodNames.
        DeactivateMethods = pcDeactivateMethods.
        MethodDeactivate = plMethodDeactivation.
        do i = 1 to extent(DeactivateMethods):
            if num-entries(DeactivateMethods[i]) > 1     
            or lookup(DeactivateMethods[i],ValidMethods) = 0 then
                undo, throw new IllegalArgumentError(subst("Deactivate Method &1 '&2' is invalid. Must be one of &2",i,DeactivateMethods[i],StringUtil:GetMessageFromEntries(ValidMethods, "or"))).    
        end.  
        /*** we need this in cases where the relation is only ued for links, but defined as temp-table in the parent
                 if extent(DeactivateMethods) = 4 and extent(MethodNames) = ? then 
            undo, throw new UnsupportedOperationError("DatasetRelation that deactivates all methods with no Method. Define a regular link and remove the relation from the dataset instead or use a constructor that has an IMethod parameter.").    
        **/      
    end constructor.
    
    method public void AddMethods(pMethods as IMethod extent):
        AddMethods(GetMethodNames(pMethods)).
    end method.
    
    method public void AddMethodName(pMethodname as character).
        AddMethods(GetSingleMethodNameAsArray(pMethodName)).
    end method.
    
    method protected void AddMethods(pcMethodNames as character extent):
        define variable cMethodNames as character extent no-undo.
        define variable iCurrentCount as integer no-undo.
      
        define variable i as integer no-undo.
        if extent(MethodNames) > 0 then
        do:
            iCurrentCount =  extent(MethodNames).
        end.
         
        extent(cMethodNames) = extent(pcMethodNames) + iCurrentCount.  
        do i = 1 to extent(cMethodNames):
            cMethodNames[i] = if i le iCurrentCount then MethodNames[i] 
                              else pcMethodNames[i - iCurrentCount].
        end. 
        
        if iCurrentcount > 0 then
            extent(MethodNames) = ?.
        
        
        MethodNames = cMethodNames.
           
    end method.
    
    method protected IJoinPredicate extent CreateJoinPredicates(phRelation as handle):
        define variable cParentFieldsString as character no-undo.
        define variable cParentFields as character extent no-undo.
        define variable cChildFieldssTRING  as character no-undo.
        define variable cChildFields  as character EXTENT no-undo.
        BufferUtil:SplitJoinFields(phRelation, false, output cParentFieldsString, output cChildFieldsString).
        cParentFields = Array:Split(cParentFieldsString).
        cChildFields = Array:Split(cChildFieldsString).
        return CreateJoinPredicates(cParentFields, cChildFields).
    end method.     
       
    method public logical Activate():
        // copied link does not have the handle 
        // copylink now converts to regular relation, but keeping this for now  
        if valid-handle(RelationHandle) then
        do:
            RelationHandle:active = true.
            return true.
        end.    
        return false.
    end.
    
    method public logical Deactivate():
        // copied link does not have the handle  
        // copylink now converts to regular relation, but keeping this for now  
        if valid-handle(RelationHandle) then
        do:
            RelationHandle:active = false.
            return true.
        end.    
        return false.  
    end.
    
    method public logical Deactivate(pRowState as RowStateEnum).
        case pRowState:
            when RowStateEnum:None then
                return Deactivate("GET").
            when RowStateEnum:Created then
                return Deactivate("POST").
            when RowStateEnum:Modified then
                return Deactivate("PUT").
            when RowStateEnum:Deleted then
                return Deactivate("DELETE").
            otherwise     
                undo, throw new UnsupportedOperationError(subst("Deactivate of RowState &1",pRowState:ToString())).   
        end case.
    end method.   
    
    method protected logical Deactivate(pcMethod as char).
        define variable i as integer no-undo.
        
        do i = 1 to extent(DeactivateMethods):
            if DeactivateMethods[i] = pcMethod then
            do:
                if valid-handle(RelationHandle) then 
                    RelationHandle:active = false.
                return true.
            end.    
        end.  
        return false.      
    end.    
    
    method public logical Deactivate(pRowState as RowStateEnum, pcServerMethod as character).
        define variable i as integer no-undo.
        define variable lFound as logical no-undo.
        if pcServerMethod > "" then
        do i = 1 to extent(MethodNames):
            if MethodNames[i] = pcServerMethod then
            do:
                if MethodDeactivate then
                    return Deactivate().
                else 
                    return Activate().
                lFound = true.    
            end.    
        end.    
        
        if not lFound then     
            return Deactivate(pRowState).
    end method. 
     
     
    method protected character extent GetMethodNames(pMethods as IMethod extent): 
        define variable i as integer no-undo.
        define variable cNames as character extent no-undo.
        extent(cNames) = extent(pMethods).
        do i = 1 to extent(pMethods): 
            cNames[i] = pMethods[i]:name.
        end.    
        return cNames.
    end method.    
    
    // hack for construcrur conversion of single methodname to array
    method protected character extent GetSingleMethodNameAsArray(pcMethodName as character): 
        define variable i as integer no-undo.
        define variable cName as character extent no-undo.
        if pcMethodName > "" then
        do: 
            extent(cname) = 1 .
            cName[1] = pcMethodname.
        end.
        return cName.
    end method.    
    
     // override to allow child table with no BE 
    method protected override character AssertAndFixPrivateEntity(pcName as character) :
        if ServiceManager:CanFindBusinessEntity(input-output pcName) then
            return pcName.
        else 
            return pcname.
    end method.
    
    method public override Object Clone():
        define variable oRelation as DatasetRelation no-undo.
        oRelation = new DatasetRelation(RelationHandle,MethodNames,MethodDeactivate,DeactivateMethods).
        oRelation:Public = Public.
        oRelation:AliasName = AliasName.
    //    oRelation:ValidationRule = ValidationRule.
        return oRelation. 
    end.    
    
    method public override character PairsList().
        // assigned in RelationHandle setter to work if stale handle  
        return FieldPairs.
    end. 
    
end class.
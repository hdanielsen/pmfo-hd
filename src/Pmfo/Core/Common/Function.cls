 
 /*------------------------------------------------------------------------
    File        : Function
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Oct 31 15:11:19 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.BusinessLogic.INamedQueryParameter from propath.
using OpenEdge.BusinessLogic.NamedQuery from propath.
using OpenEdge.BusinessLogic.NamedQueryParameter from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.DateArrayHolder from propath.
using OpenEdge.Core.DateHolder from propath.
using OpenEdge.Core.DateTimeArrayHolder from propath.
using OpenEdge.Core.DateTimeHolder from propath.
using OpenEdge.Core.DateTimeTzArrayHolder from propath.
using OpenEdge.Core.DateTimeTzHolder from propath.
using OpenEdge.Core.DecimalArrayHolder from propath.
using OpenEdge.Core.LogicalArrayHolder from propath.
using OpenEdge.Core.LogicalValue from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Core.StringArray from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.
using Pmfo.Core.Common.IFunction from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.ISignature from propath.
using Pmfo.Core.Common.Method from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.ServiceInfo from propath.
using Progress.Lang.ParameterList from propath.


class Pmfo.Core.Common.Function inherits Method implements IFunction: 
    
    define public override property BaseTypeName  as character no-undo init "Function" get.  
    define protected property NamedParameters  as INamedQueryParameter extent get. set.
    define protected variable SignatureNumber as integer no-undo.
    
    // allow isPrimitive setter to check if this is set by code 
    // (TODO) consider add subclass for primitive to manage differences instgewd of property
    define private variable BindingIsSet as logical no-undo. 
    
    define public property Binding    as MethodBindingEnum no-undo 
        get():
            if not valid-object(Binding) then 
                // Default collection - functions are typically used for complex queries of a collection
                // where a key makes no sense
                return MethodBindingEnum:Collection.  
            return Binding.    
        end. 
        set(pBinding as MethodBindingEnum):
            if isCreated then 
                undo, throw new IllegalOperationError("Cannot set Binding after the function has been invoked").
            Binding = pBinding.  
            BindingIsSet = true. // set flag to use for isPrimitive setter 
        end set.     
    
    define public override property IsPrimitive as logical no-undo
        get. 
        set(pPrimitive as log):
            // if true then set Binding to entity unless Binding already set by code
            if pPrimitive and BindingIsSet = false then
                Binding = MethodBindingEnum:Entity.
            IsPrimitive = pPrimitive.
        end. 
     
    constructor public Function ( input pName as character, input pSignatures as ISignature extent ):
        super (input pName, input pSignatures).
    end constructor.

    constructor public Function ( input pName as character, input pSignatures as ISignature extent, input plObject as logical ):
        super (input pName, input pSignatures, input plObject).
    end constructor.

    constructor public Function ( input pName as character ):
        super (input pName).
    end constructor.

    constructor public Function ( input pName as character, input plObject as logical ):
        super (input pName, input plObject).
    end constructor.
        
    constructor public Function ( input pName as character, input pParams as IParameter extent ):
        super (input pName, input pParams).
    end constructor.
        
    constructor public Function ( input pName as character, input pParams as IParameter extent, input plObject as logical ):
        super (input pName, input pParams, input plObject).
    end constructor.

    constructor public Function ( input pName as character, input pParam as IParameter):
        super (input pName, input pParam).
    end constructor.
    
    method public INamedQuery CreateNamedQuery(pExpressions as character,pAliases as IStringStringMap):
        define variable cDummy as character no-undo.
        return CreateNamedQuery(pExpressions, pAliases, output cDummy).
    end method. 
         
    method public INamedQuery CreateNamedQuery(pExpressions as character,pAliases as IStringStringMap, output pcUsedAliases as character):
        pcUsedAliases = SetParameterExpression(pExpressions,pAliases).
        return CreateNamedQueryInternal(true). // true - pass NamedParameters to new NamedQuery
    end method. 
        
    method public INamedQuery CreateNamedQuery(pStringMap as IStringStringMap):
        SetParameters(pStringMap,false). // not quoted
        return CreateNamedQueryInternal(true). // true - pass NamedParameters to new NamedQuery
    end method.
    
    method public INamedQuery CreateNamedQuery():
        PostSetValues(""). // need to signal done for no parameters
        return CreateNamedQueryInternal(false). // /false - no NamedParameters in NamedQuery
    end method. 
    
    // somewhat ugly.. the only purpose is to allow a subclass to only have one override for a
    //  NamedQuerty subclass instead of overriding multiple overloads of the public CreateNamedQuery     
    method protected INamedQuery CreateNamedQueryInternal(plNamedParameters as logical):
        define variable oParameterList as ParameterList no-undo. 
        oParameterList = new ParameterList(if plNamedParameters then 2 else 1). 
        oParameterList:SetParameter(1, "character","input", Name).
        if plNamedParameters then
            oParameterList:SetParameter(2, get-class(INamedQueryParameter):Typename,"input", NamedParameters).
        return cast(NamedQueryClass():new(oParameterList),INamedQuery).
    end.
    
    // allow sub class override for extended NamedQuert
    method protected Progress.Lang.Class NamedQueryClass():
        return get-class(NamedQuery).
    end method.    
    
    method protected override integer PrepareParameters(piNum as integer).
        extent(NamedParameters) = ?.
        if piNum > 0 then
            extent(NamedParameters) = piNum.
        return 1.     
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as character):
        // Pmfo.Core.Common.CharacterHolder - this is used in reflection GetMethod so we need to distinguish 
        // character and longchar
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new pmfo.Core.Common.CharacterHolder(pValue)).
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as longchar):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new String(pValue)).       
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as integer):
        // Pmfo.Core.Common.IntegerHolder - this is used in reflection GetMethod so we need to ditiguish 
        // int64 and integer
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new Pmfo.Core.Common.IntegerHolder(pValue)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as int64):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new OpenEdge.Core.Integer(pValue)). 
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as decimal):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new OpenEdge.Core.Decimal(pValue)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as logical):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new LogicalValue(pValue)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as date):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateHolder(pValue)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as datetime):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateTimeHolder(pValue)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as datetime-tz):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateTimeTzHolder(pValue)). 
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as character extent):
        // Pmfo.Core.Common.CharacterHolder - this is used in reflection GetMethod so we need to distinguish 
        // character and longchar
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new Pmfo.Core.Common.CharacterArrayHolder(pValues)).
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as longchar extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new StringArray(pValues)).       
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as integer extent):
        // Pmfo.Core.Common.IntegerArrayHolder - this is used in reflection GetMethod so we need to ditiguish 
        // int64 and integer
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new Pmfo.Core.Common.IntegerArrayHolder(pValues)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as int64 extent):
                                                                                 // use core for int64 
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new OpenEdge.Core.IntegerArrayHolder(pValues)). 
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as decimal extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DecimalArrayHolder(pValues)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as logical extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new LogicalArrayHolder(pValues)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as date extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateArrayHolder(pValues)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as datetime extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateTimeArrayHolder(pValues)). 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as datetime-tz extent):
        NamedParameters[piPosition] = new NamedQueryParameter(pParameter:Name,new DateTimeTzArrayHolder(pValues)). 
    end method.
   
    
    method protected override void AssertSingleObject(plSingle as logical).
       if IsPrimitive then 
       do: 
           if plSingle = false and Binding = MethodBindingEnum:Entity then 
                undo, throw new IllegalArgumentError(subst("SingleObject cannot be set to false in &1 function which is defined as IsPrimitive when Binding to single Entity",name)) .
       end.       
    end method.    
    
    method protected override character DescribeSignaturePath(pcPath as character, piSignature as integer,phBuffer as handle):
        define variable cKeys as character no-undo.
        define variable cParams as character no-undo.
        define variable i as integer no-undo.
        if MethodBindingEnum:Entity:Equals(Binding) then 
        do:
            cKeys =  ServiceInfo:GetPublicKeys(phBuffer).
            if num-entries(cKeys) = 1 and cKeys <> ""  then 
               cParams = subst("@&1",cKeys).
            else do i = 1 to num-entries(cKeys):
                cParams = cParams
                        + subst("&1=@p&2",entry(i,cKeys),i)
                        + if i = num-entries(cKeys) then "" else ",". 
            end.
            return subst("&1(&2)/&3",pcPath,cParams,GetPublicLinkTemplate(piSignature)).
        end.
        else 
            return subst("&1/&2",pcPath,GetPublicLinkTemplate(piSignature)).
    end method.    
    
     
   
end class.
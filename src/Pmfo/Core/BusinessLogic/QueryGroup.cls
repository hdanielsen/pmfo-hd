 
 /*------------------------------------------------------------------------
    File        : QueryGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 02 17:56:19 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

class Pmfo.Core.BusinessLogic.QueryGroup inherits Pmfo.Core.BusinessLogic.QueryEntry implements IQueryGroup: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
     /* Returns the array of query predicates and nested query groups */
    define public property Entries as IQueryEntry extent no-undo get. set.
    
    
    
    constructor public QueryGroup ():
        super ().
    end constructor.
            
    constructor public QueryGroup (pEntries as IQueryEntry extent):
        super ().
        Assert:HasDeterminateExtent(pEntries,"Entries").
        Entries = pEntries.
    end constructor.
    
    constructor public QueryGroup (pEntry as IQueryEntry):
        super ().
        Assert:NotNull(pEntry,"Entry").
        extent(Entries) = 1.
        Entries[1] = pEntry.
    end constructor.
    
    constructor public QueryGroup (pJoin as JoinEnum, pEntries as IQueryEntry extent):
        this-object (pentries).
        this-object:Join = pJoin. 
    end constructor.
    
    constructor public QueryGroup (pJoin as JoinEnum, pEntry as IQueryEntry):
        this-object (pentry).
        this-object:Join = pJoin. 
    end constructor.
    
    method public logical HasEntry(pQueryEntry as IQueryEntry):
        define variable i      as integer no-undo.
        do i = 1 to extent(Entries):
            if Entries[i]:Equals(pQueryEntry) then 
                return true.
        end.
        return false.
    end method.
    
    method public logical RemoveEntry(pQueryEntry as IQueryEntry):
        define variable oEntries as IQueryEntry extent no-undo.
        define variable i      as integer no-undo.
        define variable iCount as integer no-undo.
        define variable iRemove as integer no-undo.
        
        if extent(Entries) > 0 then
        do:
            extent(oEntries) = extent(Entries) - 1.
            do i = 1 to extent(Entries):
               if Entries[i]:Equals(pQueryEntry) then 
               do:
                   iRemove = i.
                   leave.               
               end.     
            end.    
            if iRemove = 0 then
                return false. 
            do i = 1 to extent(Entries):
               if i <> iRemove then
               do:
                   iCount = iCount + 1.
                   oEntries[iCount] = Entries[i].
               end.    
            end.
            extent(Entries) = ?.
            Entries = oEntries.
        end.
        else do: 
            return false.
        end.
            
        
    end method.
    
    method public logical AddEntry(pQueryEntry as IQueryEntry):
        define variable oEntries as IQueryEntry extent no-undo.
        define variable i as integer no-undo.
        define variable lAnyGroup as logical no-undo.
        define variable oSubGroup as QueryGroup no-undo.
        if pQueryEntry = this-object then 
            undo, throw new IllegalArgumentError("Cannot add itself as entry").
        if extent(Entries) > 0 then
        do:
            extent(oEntries) = extent(Entries) + 1.
            do i = 1 to extent(Entries):
               oEntries[i] = Entries[i].
               if type-of(oEntries[i],Ccs.BusinessLogic.IQueryGroup) then 
                  lAnyGroup = true. 
            end.
            extent(Entries) = ?.
            oEntries[extent(oEntries)] = pQueryEntry.
            Entries = oEntries.
        end.
        else do:
            extent(Entries) = 1.
            Entries[1] = pQueryEntry.
        end.    
        return true.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pcValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,piValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,plValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pdValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as date).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as datetime).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as datetime-tz).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pcValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,piValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,plValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValueHolder as IPrimitiveArrayHolder).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(pcfield,pOperator,pValueHolder). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pcValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,piValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,plValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as date).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime-tz).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate = new QueryPredicate(poJoin,pcfield,pOperator,pValue). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pcValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,piValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,plValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
     method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate =new QueryPredicate(poJoin,pcfield,pOperator,pdValues). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValueholder as IPrimitiveArrayHolder).
        define variable oQueryPredicate as IQueryPredicate no-undo.
        oQueryPredicate = new QueryPredicate(poJoin,pcfield,pOperator,pValueholder). 
        if AddEntry(oQueryPredicate) then 
            return oQueryPredicate.
    end method.
    
    
    method public logical AnyOr():
        define variable i      as integer no-undo.
        define variable oEntry as IQueryEntry no-undo.
        do i = 1 to extent(Entries):
            case Entries[i]:Join:
                when JoinEnum:Or or when JoinEnum:OrNot then 
                    return true.
            end.    
        end.    
        return false.
        
    end method.
    
    method public override character ToString(phbuffer as handle, plUseSerializername as logical ):
        return ToString(phbuffer,plUseSerializername,false).
    end method.
        
    method public override character ToString(phBuffer as handle,plUseSerializername as logical, plTrimParenthesis as logical):
        define variable i           as integer no-undo.
        define variable cExpression as character no-undo.
        define variable cJoin       as character no-undo.
        
        do i = 1 to extent(Entries):
            do on error undo, throw:
                cJoin = lc(QueryUtil:JoinString(Entries[i]:Join,i = 1)).
                catch e as Progress.Lang.Error :
                   cJoin = "?".     
                end catch.
            end.
            if cJoin > ""  then 
                cJoin = " " + cJoin + " ".
            
            if valid-handle(phBuffer) and type-of(Entries[i],Pmfo.Core.BusinessLogic.IQueryEntry) then
            do:    
                cExpression = cExpression 
                            + cJoin
                            + cast(Entries[i],Pmfo.Core.BusinessLogic.IQueryEntry):ToString(phbuffer,plUseSerializername).
            end.
            else do:
                cExpression = cExpression 
                            + cJoin
                            + Entries[i]:ToString().
                          .
            end.                
        end.    
        if plTrimParenthesis = false then
            cExpression =  subst("(&1)",cExpression).
            
        return cExpression.       
    end method.
    
    method override public character ToString(  ):
        return ToString(?,false). 
    end method.
     
end class.
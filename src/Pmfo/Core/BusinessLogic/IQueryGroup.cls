
/*------------------------------------------------------------------------
    File        : IQueryGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 22:09:24 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.BusinessLogic.IQueryGroup.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.

interface Pmfo.Core.BusinessLogic.IQueryGroup inherits IQueryGroup, Pmfo.Core.BusinessLogic.IQueryEntry:  
    method public logical RemoveEntry(pQueryEntry as IQueryEntry).
    method public logical AddEntry(pQueryEntry as IQueryEntry).
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as date).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime-tz).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValue  as IPrimitiveHolder).
    
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
    method public IQueryPredicate AddEntry(pcField as character,pOperator as QueryOperatorEnum,pValueHolder  as IPrimitiveArrayHolder).
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as date).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime-tz).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue  as IPrimitiveHolder).
    
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
    method public IQueryPredicate AddEntry(poJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValueHolder as IPrimitiveArrayHolder).
    
    method public logical AnyOr().
end interface.
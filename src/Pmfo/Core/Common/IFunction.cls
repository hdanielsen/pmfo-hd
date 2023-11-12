
/*------------------------------------------------------------------------
    File        : IFunction
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Oct 31 15:10:21 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.INamedQuery from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.

interface Pmfo.Core.Common.IFunction inherits IMethod:
   
  
    define public property Binding          as MethodBindingEnum no-undo get. set.  
         
    method public INamedQuery CreateNamedQuery(pcExpressions as character, input pAliases as IStringStringMap).       
    // convert the parameters from the web url to a NamedQeury characters and dates should be quoted
    method public INamedQuery CreateNamedQuery(pcExpressions as character, input pAliases as IStringStringMap, output pcUsedAliases as character).  
    // to ude from code - characters and dates should not be quoted - dates must be iso-date
    method public INamedQuery CreateNamedQuery(pStringMap as IStringStringMap):
    // use for no parameters - defualt is to return aa NamedQuery with the function name
    // but functiona can overide for redirect (See ReportFunction)     
    method public INamedQuery CreateNamedQuery().
    
         
end interface.
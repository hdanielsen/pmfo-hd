
/*------------------------------------------------------------------------
    File        : IParameterTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Dec 15 01:33:29 EST 2020
    Notes       : Interface that defines and sets parameters 
                  Used by ParameterExpressionParser 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.StringStringMap from propath.

interface Pmfo.Core.Common.IParameterTarget:
      
    method public void ProcessRequest(pParameters as StringStringMap).
   
    
end interface.
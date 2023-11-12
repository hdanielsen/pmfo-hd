 
 /*------------------------------------------------------------------------
    File        : ServiceDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 06 16:32:56 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.IServiceDataAccess.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Common.DataService from propath.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.ServiceDataAccess abstract inherits DataService implements IServiceDataAccess: 

    define public property LegacyName as character no-undo 
    get.
    
    protected set(pcname as character):
        if index(pcname,".") > 0 then
            undo, throw new IllegalArgumentError("Set LegacyName with period").
        
        if index(pcname,"/") > 0 then
            undo, throw new IllegalArgumentError("Set LegacyName with forward slash").
        
        if index(pcname,StringConstant:BACKSLASH) > 0 then
            undo, throw new IllegalArgumentError("Set LegacyName with backslash").
        LegacyName = pcName.
            
    end. 

    define public abstract property Resource as character no-undo 
    get. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ServiceDataAccess (  ):
        super ().
    end constructor.
    
    constructor public ServiceDataAccess (pcLegacyName as character  ):
        super ().
        LegacyName = pcLegacyName.
    end constructor.
    
    method public override void Destroy(  ): 
    end.
    
    method public override void Initialize(  ).
    end.
    
end class.
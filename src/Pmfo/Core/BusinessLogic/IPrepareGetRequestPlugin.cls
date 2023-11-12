
/*------------------------------------------------------------------------
    File        : IPrepareGetRequestPlugin
    Purpose     : Intgerface for plugin for GetDataRequest preparation   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 12 20:13:11 EDT 2023
    Notes       : Kind of worakraound for the fact the Be PrepareGetRequest is overridable
                  and used in concrete instancesm, but general behavior is needed for all
                  (and er shoild not need to call super in the instances)
                  
                  While we typically would make the override final in an application 
                  specific business Entity and then call an override with 
                  application specific IGetDataRequest sub interface from there 
                  it was bit risky when the need was realized/ undestood       
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.BusinessLogic.IPrepareGetRequestPlugin:  
    method public void PrepareGetRequest (poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, phDataset as handle).
end interface.
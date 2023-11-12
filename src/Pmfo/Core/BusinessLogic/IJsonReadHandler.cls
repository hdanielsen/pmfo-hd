
/*------------------------------------------------------------------------
    File        : IJsonReadHandler
    Purpose     : Intended for BusinessEntities that need extra attention 
                  or support when json is loaded, but still want the main 
                  behavior of the JsonReader    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 15 08:31:18 EDT 2020
    Notes       : This is currently not default BE behavior but this should be 
                  evaluated. Both methods can return false to keep defaukt behavior   
               -  There is an attempt to have as little knowledge as possible 
                  about the WEB transport's specific message operations in the 
                  BE itself and vice versa
               -  The existance of this interface also triggers use of the 
                  JsonReader on POST for cases where it would not otherwise 
                  do this.   
               -  This interfacer may be extended to support other future needs
               -  TODO: 
                  The NextIdent for keys should possibly also be handled 
                  here
----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.BusinessLogic.IJsonReadHandler:  
    
    /** read a buffer field from json - 
        return true if the read was handled/done
        if false then the JsonReader will use the default read   
    */
    method public logical ReadField( pjson as JsonObject,phfld as handle).
    
    // will be called for fields in buffer that are serializable but not found in json
    // can be utilized to assign a key/sequence to avoid unique index error 
    // for data that is intended to be assigned on server  
    method public void HandleFieldNotInJson(pjson as JsonObject, phfld as handle).
    
    // prepare for load of buffer (for example load support data needed for ResdField) and  
    // return true if read only table (skip mark state)
    method public logical CheckReadOnlyAndPrepareBuffer( phBuffer as handle, pState as RowStateEnum).
    
    
    
end interface.
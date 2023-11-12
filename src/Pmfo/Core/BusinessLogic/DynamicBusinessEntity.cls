 
 /*------------------------------------------------------------------------
    File        : DynamicBusinessEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 26 14:10:49 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.TableUtil from propath.
using Pmfo.Core.Service.IClientRequest from propath.

class Pmfo.Core.BusinessLogic.DynamicBusinessEntity inherits BusinessEntity: 
/*    constructor public DynamicBusinessEntity ( pcSerializeName as character ):*/
/*      //  super (input CreateDatasetAndTable(pcSerializeName)).               */
/*        ClientOnly = true.                                                    */
/*        //Resource = pcserializename.                                         */
/*    end constructor.                                                          */
/*                                                                              */

    
    constructor public DynamicBusinessEntity ( phDataset as handle, pcSerializeName as character, phKeys as handle extent ):
        super (input CreateDatasetAndTable(phDataset,pcSerializeName,phKeys)).
        ClientOnly = true.
        //Resource = pcserializename.
    end constructor.
    
    constructor public DynamicBusinessEntity ( phDataset as handle, pcSerializeName as character, phKey as handle  ):
        super (input CreateDatasetAndTable(phDataset,pcSerializeName,Array:ToArray(phKey))).
        ClientOnly = true.
        //Resource = pcserializename.
    end constructor.
      // Returns a separated list of resources that is to be exposed as a common childlink to a BE. 
    // Currently used to allow reportBE to be actions and methods to be used and linked to any BE 
    method public override character GetFixedChildren(pClientRequest as IClientRequest):
        return "".
    end method. 
    
    method public override character GetContainerRedirect(pClientRequest as IClientRequest, output pcContext as character).
        return "".
    end method.
    
    method protected handle CreateDatasetAndTable(phDataset as handle, pcserializename as character, phKeys as handle extent):
        define variable oBe as IBusinessEntity  no-undo.
        define variable cKeys as character   no-undo.
        define variable hSourceBuffer  as handle no-undo.
        define variable hbuffer as handle no-undo.
        define variable hTable as handle no-undo.
        define variable cKeyArray as character extent no-undo.
        define variable hdataset as handle no-undo.
        define variable hNewParent as handle no-undo. 
        define variable pFieldPairs as character no-undo.
       // define variable hCopyFields as handle extent no-undo.
        define variable i as integer no-undo.
      //  define variable hDataset as handle no-undo.
        oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(pcserializename).
        
        hSourceBuffer = oBe:Datasets[1]:get-buffer-handle(1).
/*        cKeys = BufferUtil:GetKeys(hSourceBuffer).*/
/*        cKeyArray = Array:SplitpcKeys).           */
       // extent(hCopyFields) = extent(cKeyArray).
/*        do i = 1 to extent(phKeys):                             */
/*            hCopyFields[i] = hBuffer:buffer-field(cKeyArray[i]).*/
/*        end.                                                    */
/*                                                                */
        hTable = TableUtil:CreateTableLike(hSourceBuffer, hSourceBuffer:name, phKeys, true, output pFieldpairs ).
        create dataset hDataset.
        hDataset:create-like(phDataset).
     //   create dataset hDataset.
        // progress limitation is 32 char
        // we could probably do this always the "-dyn" is added for (memory leak( debugging purposes only 
/*        hDataset:name = "Ds" + pcSerializename. // onot used iother than in memopry leak check programs...*/
/*        hDataset:serialize-name = hSourceBuffer:serialize-name.                                           */
/*                                      
                                                                             */
        hNewParent = hDataset:get-top-buffer (1).
        
        create buffer hBuffer for table hTable buffer-name hSourceBuffer:name + "-dyn".
        
        hBuffer:serialize-name = hSourceBuffer:serialize-name.
        hDataset:add-buffer(hBuffer).
        
        hDataset:add-relation(hNewParent,hBuffer,pFieldpairs,false,true).
      return hDataset.
      //  return hDataset.          
    end method.     
    
    
    
end class.
 
 /*------------------------------------------------------------------------
    File        : Pmfo.Core.BusinessLogic.MemoryObjectBE
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 10/07/2024 -moved up from application
    Notes       :  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.ClientBusinessEntity from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.

class Pmfo.Core.BusinessLogic.MemoryObjectBE abstract inherits ClientBusinessEntity: 
    {Pmfo/Core/schema/memoryObject.i}
        
    define dataset dsMemoryObject for ttMemoryObject
          // data-relation dataset-buffer for ttMemoryObject,ttMemoryObject relation-fields(handle,parenthandle) recursive
          .
        
    constructor public MemoryObjectBE (   ):
        super (dataset dsMemoryObject:handle).
        
        ServerType = ServerTypeEnum:Report or ServerTypeEnum:General. 
    end constructor.

    method public override int64 CreateSourceRecords(output table-handle phTable):
        return CreateMemoryObjects(output table-handle phTable by-reference).
    end method.
         
    method protected int64 CreateMemoryObjects(output table ttMemoryObject).
        define variable hObject as handle no-undo.
        define variable iCount as integer no-undo.
        define variable iSessionId as integer no-undo.
        
        iSessionId =  session:current-request-info:SessionID no-error.
        
        hObject = session:first-dataset.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle     = hObject
                ttMemoryObject.name       = if hObject:name > "" then hObject:name else string(hObject)
                ttMemoryObject.publicName = hObject:serialize-name
                ttMemoryObject.type       = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
                ttMemoryObject.info       = hObject:private-data 
                ttMemoryObject.sessionId = iSessionId
                iCount = iCount + 1
                ttMemoryObject.zz_seq     = iCount
                
                
                .
            hObject = hObject:next-sibling.
        end.
        
        // variables to workaround crash when referencing stale data in  relation:parent-buffer.
        define variable hparentrelation as handle no-undo.
        define variable hDataset        as handle no-undo.
        define variable i as integer no-undo. 
        define variable lSkipparent as logical no-undo.  
        
        hObject = session:first-buffer.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if hObject:name > "" then hObject:name else string(hObject)
                ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.sessionId    = iSessionId
                hparentrelation             = hObject:parent-relation.
                lSkipParent = false.
            if valid-handle(hParentRelation) then 
            do:
               hDataset = hObject:dataset.      
               do i = 1 to hdataset:num-buffers:
                 if not valid-handle(hdataset:get-buffer-handle(i)) then
                 do: 
                     ttMemoryObject.publicParentName = ?.
                     ttMemoryObject.problem = subst("The dataset buffer &1 is stale. PublicParentName cannot be resolved.",i).   
                     lSkipParent = true. // just referencing parent-relation caused issues (I think... needs research) 
                     leave.
                 end.
               end.
               if not lSkipParent then          
                   ttMemoryObject.publicParentName = hParentRelation:parent-buffer:serialize-name.

            end.
            assign     
                ttMemoryObject.type         = hObject:type
                ttMemoryobject.parentHandle = hObject:dataset
                ttMemoryObject.createdBy  =  (if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else "")
                ttMemoryObject.info       =  (if hObject:private-data > "" then hObject:private-data
                                              else if valid-handle(hObject:table-handle)
                                              then if hObject:table-handle:has-records then "Has Records"
                                                   else "No Records"
                                              else ""      
                                             )
                                             
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-data-source.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if hObject:name > "" then hObject:name else string(hObject)
            //    ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
                ttMemoryObject.sessionId = iSessionId                                  
                
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-query.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if hObject:name > "" then hObject:name 
                                              else if valid-handle(hObject:get-buffer-handle(1)) then  hObject:get-buffer-handle(1):name
                                              else string(hObject)
             //   ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
                ttMemoryObject.info = hObject:prepare-string
                ttMemoryObject.sessionId = iSessionId                                  
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-child.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = hObject:name 
             //   ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
              //  ttMemoryObject.info = ??  
                ttMemoryObject.sessionId = iSessionId                                  
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-server.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if can-query(hObject,"name") then hObject:name else "" 
             //   ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
               // ttMemoryObject.info  ..??
                ttMemoryObject.sessionId = iSessionId                                  
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-server-socket.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if can-query(hObject,"name") then hObject:name else ""  
             //   ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
             //   ttMemoryObject.info = ??  
                ttMemoryObject.sessionId = iSessionId                                  
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        hObject = session:first-socket.
        do while valid-handle(hObject):
            create ttMemoryObject.
            assign 
                ttMemoryObject.handle       = hObject
                ttMemoryObject.name         = if can-query(hObject,"name") then hObject:name else "" 
             //   ttMemoryObject.publicName   = hObject:serialize-name
                ttMemoryObject.type         = hObject:type
                ttMemoryObject.createdBy  =  if valid-handle(hObject:instantiating-procedure) then hObject:instantiating-procedure:file-name else ""
             //   ttMemoryObject.info = ??  
                ttMemoryObject.sessionId = iSessionId                                  
                iCount = iCount + 1
                ttMemoryObject.zz_seq    = iCount
                .
            hObject = hObject:next-sibling.
        end.
        
        return iCount.
    end method.
    
     
end class.
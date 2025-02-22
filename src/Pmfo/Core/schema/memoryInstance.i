/*------------------------------------------------------------------------------
 Purpose     : 
 Syntax      : 
 Description :  
 Author(s)   : hdaniels
 Created     : 04/072023
------------------------------------------------------------------------------*/
define temp-table ttMemoryInstance no-undo serialize-name "memoryInstances" {1}
   field type                             as character   serialize-name "type"
   field toString                         as character   serialize-name "toString"
   field ref                              as int64       serialize-name "objectId"
   field hasStatics                       as logical     serialize-name "hasStatics"
   field publicName                       as character   serialize-name "publicName"
   field info                             as character   serialize-name "info"
   field sessionId                        as integer     serialize-name "sessionId"
   field zz_seq                           as int64       serialize-hidden
   index idx  ref 
   index zz_seq as primary zz_seq
   .
/*------------------------------------------------------------------------------
 Purpose     : 
 Syntax      : 
 Description :  
 Author(s)   : hdaniels
 Created     : 09/11/2020
------------------------------------------------------------------------------*/
define temp-table ttMemoryObject no-undo serialize-name "memoryObjects" {1}
   field name                             as character   serialize-name "name"
   field publicName                       as character   serialize-name "publicName"
   field publicParentName                 as character   serialize-name "publicParentName"
   field handle                           as handle      serialize-name "handle"   
   field parentHandle                     as handle      serialize-name "parentHandle"   
   field createdBy                        as character   serialize-name "createdBy"   
   field info                             as character   serialize-name "info" 
   field problem                          as character   serialize-name "problem" 
   field type                             as character   serialize-name "type"
   field sessionId                        as integer     serialize-name "sessionId"
   field zz_seq                           as int64       serialize-hidden
   index idx as unique name handle
   index zz_seq as primary zz_seq
   .
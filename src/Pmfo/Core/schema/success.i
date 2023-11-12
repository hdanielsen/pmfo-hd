/*------------------------------------------------------------------------------
 File        : schema/info.i
 Purpose     : 
 Syntax      : 
 Description :  
 Author(s)   : hdaniels
 Created     : 02/22/2021 
 Notes       : Info from submit   
------------------------------------------------------------------------------*/
define temp-table ttSuccess no-undo serialize-name "@success" 
   field entity                           as character   serialize-name "entity"
   field seq                              as integer     serialize-hidden 
   field msg                              as character   serialize-name "message"
   field urlId                            as character   serialize-name "urlId" 
   field key                              as Progress.Lang.Object      serialize-name "key"
   field stringkey                        as character serialize-hidden serialize-name "stringKey"
   index idx as unique entity stringkey
   index idxseq as primary seq.
   
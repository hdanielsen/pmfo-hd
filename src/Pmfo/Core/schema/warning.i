/*------------------------------------------------------------------------------
 File        : schema/warning.i
 Purpose     : 
 Syntax      : 
 Description :  
 Author(s)   : hdaniels
 Created     : 08/08/2019 
 Notes       : Warnings from submit   
------------------------------------------------------------------------------*/
define temp-table ttWarning no-undo serialize-name "@warnings" {1} 
   field entity                           as character   serialize-name "entity"
   field seq                              as integer     serialize-hidden 
   field msg                              as character   serialize-name "message"
   field fieldName                        as character   serialize-name "field"
   index idx as primary unique seq entity.
   
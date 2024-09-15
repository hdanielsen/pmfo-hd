
/*------------------------------------------------------------------------
    File        : businessEntity.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Thu Mar 21 17:57:02 EDT 2019
    Notes       : application business entities  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttEntity  no-undo serialize-name "entities" {1} before-table biEntity
    field CanCreate        as logical
    field CanDelete        as logical
    field ClassName        as character
    field ClientOnly       as logical  
    field EntityReferences as character serialize-hidden
    field IsDataContainer  as logical serialize-hidden
    field MustInitializeForTableRead as logical
    field KeyList          as character
    // format for debug and test  longest name 6/18/2024 - 53   
    field Resource         as character format "x(55)" 
    field Public           as logical
    field ReadOnly         as logical  
    field ServerTypes      as character  
    field SubscribeGet     as logical
    field SubscribeReceive as logical
    field SubscribeUpdate  as logical
    field SubscribeAction  as logical
    field TypeName         as character
    
    field zz_seq           as integer serialize-hidden 
index resource  as  unique Resource
index type TypeName Resource
index className as unique ClassName.

    

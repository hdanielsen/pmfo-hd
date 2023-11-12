
/*------------------------------------------------------------------------
    File        : entityName.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     :    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define {&mode} temp-table ttEntityName serialize-name "entityNames" 
     field category          as character  // SchemaTypeEnum:G 
     field name              as character
     field sourceName        as character
 //    field zz_seq            as int64       serialize-hidden
     index name              as primary unique name  
     index idx               as unique category sourceName
   //  index zz_seq as primary zz_seq 
     .
      
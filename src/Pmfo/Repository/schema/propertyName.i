
/*------------------------------------------------------------------------
    File        : propertyName.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     :    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define {&mode} temp-table ttPropertyName serialize-name "propertyNames"
     field sourceName        as character
     field name              as character
  //   field zz_seq            as int64       serialize-hidden
     index name              as primary unique name
     index idx               as unique sourceName
    // index zz_seq as primary zz_seq 
     .
      
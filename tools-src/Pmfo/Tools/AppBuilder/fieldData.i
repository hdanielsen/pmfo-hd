
/*------------------------------------------------------------------------
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Fri Feb 15 12:06:28 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
 define temp-table fieldData  no-undo {1} before-table beforeFieldData  
    field isExcluded     as logical   label "Exclude"
    field fieldName      as character label "Field Name"
    field serializeName  as character label "Serialize Name"
    field dataType       as character label "Data Type"
    field isCalc         as logical   label "Calc"
    field inSDO          as logical   label "SDO"
    field isJoin         as logical   label "Join"
    
    index idxName  is primary fieldName.
    
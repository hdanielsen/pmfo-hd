
/*------------------------------------------------------------------------
    File        : dataAccess.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Thu Mar 21 17:57:02 EDT 2019
    Notes       : application data access  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttDataAccess  no-undo serialize-name "dataAccess" {1} before-table biDataAccess    
    field Resource   as character
    field ClassName  as character
    field TypeName   as character
    field LegacyName as character init ?
index resource  as primary unique Resource
index type TypeName Resource
index legacy as unique LegacyName
index className as unique ClassName.
    

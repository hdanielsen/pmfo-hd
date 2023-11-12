
/*------------------------------------------------------------------------
    File        : dataSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Thu Mar 22 12:00 EDT 2019
    Notes       : application data sources  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttDataSource  no-undo serialize-name "dataSources" {1} before-table biDataSource
    field EntityName     as character format "x(32)"
    field TableName      as character format "x(32)"
    field TypeCode       as character format "x(60)"
    // optional - managed by ServiceManager TypeKey property
    field TypeKey        as integer   // serialize-hidden //serialize-name "TypeKey" 
    field Type           as character format "x(9)"
    field ClassName      as character format "x(50)"
index entity as primary unique EntityName
index className ClassName
index type type
index typekey typekey
index typecode typecode
index sourceid TableName TypeKey. 

    

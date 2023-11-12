
/*------------------------------------------------------------------------
    File        : lookup.i
    Purpose     : 

    Syntax      :

    Description : 
    Author(s)   : hdaniels
    Created     : Thu Mar 22 12:00 EDT 2019
    Notes       : application lookups - subset of datasource, 
                  witnout 
                  - table - used to detect and create datasources that do not have a normal table,
                            but is stored in an application specific lookup table     
                  - type  - will create datasource with  type = 'CodeTable'    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define {&mode} temp-table ttLookupType  no-undo serialize-name "lookupTypes" {1} before-table bittLookupType
    field TypeCode       as character format "x(60)" serialize-name "typeCode"
    field TypeKey        as integer   init ?  serialize-name "typeKey" 
    
    field ClassName      as character format "x(50)" serialize-name "className"
    // somewhat reduntant since it is first lower case version of Classname,
    // but we currently use ClassName in buffer-copy to resource in some tools 
    // and model Type for UI
    // subclasses that are used by UI will switch the serialize-hidden   
    field ModelType      as character format "x(50)" serialize-hidden serialize-name "modelType" 
    field EntityName     as character format "x(32)" serialize-name "dataService"
    field DisplayName    as character format "x(60)" serialize-hidden serialize-name "displayName"   
index typeCode as primary unique TypeCode
index typekey as unique typekey
index entity entityName  
index className ClassName
.
    

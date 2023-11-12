&if defined(codetablekey-datatype) = 0 &then
&scop codetablekey-datatype integer
&endif

&if defined(codetablekey-format) = 0 &then
&scop codetablekey-format ">>>"
&endif


/*------------------------------------------------------------------------
    File        : filedata.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Fri Feb 15 12:06:28 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
 define {&mode} temp-table resourceData  no-undo {1} before-table beforeResourceData
    field resourceNum     as int64     label "Resource Key"  
    field resourceName    as character label "Resource"
    field entityName      as character label "Entity"
    field className       as character label "Class Name" 
    field tableName       as character label "Table Name"
    field tempTableName   as character label "Temp-Table Name"
    field beforeTableName as character label "Before-Table Name"
    field databaseName    as character label "Database Name"
    field sdoName         as character label "SDO"
    field dlpName         as character label "DLP"
  //  field codeTableKey    as integer   format ">>>>9"  label "Code Table Key"
    
    field codeTableKey    as {&codetablekey-datatype} format {&codetablekey-format}  label "Code Table Key"
    field codeTableName   as character label "Code Table Name"
    field noCodes         as logical   label "No Data"
    field businessEntity  as character label "Business Entity"
    field includefile     as character label "Include File"
    field dataAccess      as character label "Data Access"
    field dataSource      as character label "Data Source"
    field tempdb          as logical   label "Temp DB"
    field NotInUse        as logical   label "Not In Use"
    field isAbstract      as logical   label "Abstract" 
    field hasParam        as logical   label "Has Parameter"
    field NotFromData     as logical   label "From Source"   // not generated from SDOs, tables or code table
    field onlyInclude     as logical   label "Only Include" // only used when notfromdata true
    field definedinDb     as logical   label "Defined" // table is set from DSDO and DLP set this flag if defined in dictionary
    field dbFields            as Progress.Lang.Object
    field codeTableFields     as Progress.Lang.Object
    field Error               as Progress.Lang.Object  
    
    field sdoFields           as Progress.Lang.Object
    field SdoModel            as Progress.Lang.Object 
    field DlpModel            as Progress.Lang.Object 
    field BusinessEntityModel as Progress.Lang.Object
    field DataSourceModel     as Progress.Lang.Object
    
    index idxResource     is primary unique resourceNum
    index idxDatasource   DataSource
    index idxBusinessEntity BusinessEntity
    index idxEntity EntityName
    index idxSdo SDOname TableName
    index idxDLp dlpname
    index idxCodeName codeTableName
    index idxCodeKey codeTableKey
    
   // index idxNotFromData NotFromData
    index idxtable tableName. 
    
define {&mode} temp-table resourceError  no-undo {1}
    field resourceNum     as int64     label "Resource Key"  
    field errorText       as char      label "Error"       
    index idxResource     is primary resourceNum.
    
    

/*------------------------------------------------------------------------
    File        : dataSourceError.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     :7/6/2025 
    Notes       : application data source error -
                  Resource json data sources are generated from source code or lookup tables 
                  with NamkeService rules to find a puplic name - The name can be hard coced or fed from 
                  publicNames (managed in publicNames.json) 
                  If the lookup table key is changed without updating NameService or PublicNames
                  the ResurceBE AddDynamicDataSources will not be able ot add ttDataSource record and 
                  we log that in this table, so it can be checked from CreateDataSource
                - NOTE that thesee are only considered errors when actually requested from ServiceManager:CreateDataSource
                  We do not add to the log for client only, but there may BEs that controls this with UpdateRequest:AddNoTargetTable
                  and there may also be BEs that uses the data source inherited from its super BE              
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttDataSourceError  no-undo serialize-name "dataSourceErrors" {1} before-table biDataSourceError
    field EntityName                     as character format "x(32)" serialize-name "entityName"
    // logged if only requested from a temp-table in another BE
    field NumContainerReferences          as integer serialize-name "numContainerReferences"  
    field IsRequestedByBusinessEntity     as logical serialize-name "isRequestedByBusinessEntity"  
index entity as primary unique EntityName. 

    

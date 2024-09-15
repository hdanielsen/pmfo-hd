
/*------------------------------------------------------------------------
    File        : dynamicsdata.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Jul 24 12:24:50 EDT 2024
    Notes       :
  ----------------------------------------------------------------------*/
    
 /* ttRequiredRecord contains the list of parameters that decide how the 
       XML file is written out. */
    define {&mode} temp-table ttRequiredRecord no-undo {&RCodeInfo}
      field iSequence       as integer   
      field cJoinFieldValue as character format "X(50)":U
      index pudx is unique primary
        iSequence
      .
    
    /* ttTable contains the data from the gsc_dataset_entity table
       for the particular dataset being processed. */
    define {&mode} temp-table ttTable no-undo  {&RCodeInfo}
      field iEntitySeq      as integer    format ">,>>9":U column-label  "Seq"
      field cEntityMnemonic as character  format "X(15)":U  column-label "Entity!Mnemonic"
      field cDatabase       as character  format "X(30)":U  column-label "DB Name"
      field cTableName      as character  format "X(30)":U  column-label "Table"
      field lPrimary        as logical    format "Yes/No":U column-label "Primary"
      field cJoinMnemonic   as character  format "X(15)":U  column-label "Join!Mnemonic"
      field cJoinFieldList  as character  format "X(50)":U  column-label "Join Fields"
      field cWhereClause    as character  format "X(50)":U  column-label "Where Clause"
      field cExcludeFields  as character  format "X(50)":U  column-label "Exclude Fields"
      field hBuffer         as handle
      index pudx is unique primary
        iEntitySeq
        cEntityMnemonic
      index dx1 
        lPrimary
      index dx2
        cJoinMnemonic
        iEntitySeq
      index dx3
        cEntityMnemonic
        lPrimary
      .
    
    /* The ttEntityList table is a working table used to make sure that the 
       temp-table is only written to the XML header section once. */ 
    define {&mode} temp-table ttEntityList no-undo {&RCodeInfo}
      field cEntityMnemonic as character  format "X(50)":U
      field cDatabase       as character  format "X(50)":U
      field cTableName      as character  format "X(50)":U
      field cJoinMnemonic   as character  format "X(50)":U
      field cJoinFieldList  as character  format "X(50)":U
      field cObjField       as character  format "X(50)":U
      field cKeyField       as character  format "X(50)":U
      field lHasObj         as logical    format "YES/NO":U
      field lVersionData    as logical
      field hBuffer         as handle
      field iTableNo     as integer 
      index pudx is unique primary
        cEntityMnemonic
      index dxJoinMnemonic
        cJoinMnemonic
      .
    
    
    /* This table is a working table that is used to manipulate the data that 
       is contained in the XML file. */
    define {&mode} temp-table ttNode no-undo   {&RCodeInfo}
      field iRequestNo    as integer
      field iLevelNo      as integer
      field iTableNo      as integer
      field cNode         as character format "X(25)"
      field iExtentIndex  as integer
      field cValue        as character format "X(50)"
      field lDelete       as logical   format "YES/NO"
      index pudx is unique primary
        iRequestNo
        iLevelNo 
        iTableNo
        cNode
        iExtentIndex
      index dxDelete 
        lDelete
      .
    
    /* This table contains a list of all the tables that have been defined
       by buildTableStruct that have not yet been returned to the client */
    define {&mode} temp-table ttTableList no-undo {&RCodeInfo}
      field iRequestNo      as integer 
      field iTableNo        as integer 
      field cEntityMnemonic as character
      field cDBName         as character format "X(30)"  column-label "Database":U
      field cTableName      as character format "X(30)"  column-label "Table Name":U
      field hTable          as handle
      index pudx is unique primary
        iRequestNo
        iTableNo
      index udx is unique
        iRequestNo
        cDBName
        cTableName
      .
    
    /* This table is used to work out if an index already exists on field combinations */
    define {&mode} temp-table ttIndexList no-undo {&RCodeInfo}
      field iIndexNo   as integer
      field cFieldList as character format "X(50)":U
      field lUnique    as logical   format "YES/NO":U
      index pudx is unique primary
        cFieldList
      index udx1 is unique
        iIndexNo
      .
    
    /* This table stores the dataset attributes applicable to each
       request */
    define {&mode} temp-table ttDSAttribute no-undo {&RCodeInfo}
      field iRequestNo   as integer
      field cAttribute   as character  format "X(50)":U
      field cValue       as character  format "X(50)":U
      index pudx is unique primary
        iRequestNo
        cAttribute
      .
    
    /* This table contains a list of handles to objects used in requests */
    define {&mode} temp-table ttReqHandle no-undo {&RCodeInfo}
      field iRequestNo      as integer
      field iDelOrder       as integer
      field cHandleName     as character  format "X(50)":U
      field cHandleType     as character  format "X(50)":U
      field hHandle         as handle
      index pudx is unique primary
        iRequestNo 
        cHandleName
      index udx is unique 
        iRequestNo 
        iDelOrder
        cHandleName
      .
    
    /* This table contains a record per dataset transaction */
    define {&mode} temp-table ttTransaction no-undo {&RCodeInfo}
      field iRequestNo       as integer   
      field iTransNo         as integer    format ">>>,>>9":U column-label "Trans No":U
      field cObjFieldVal     as character  format "X(30)":U   column-label "Object Field":U
      field cKeyFieldVal     as character  format "X(30)":U   column-label "Key Field":U
      field rRowid           as rowid
      index pudx is unique primary
        iRequestNo
        iTransNo
      .
    
    /* This table stores the list of version records for the data that is currently
       being written to the XML file. When we have finished exporting the XML file and
       the file has been successfully saved, this table is used to determine which records
       need to have their version status re-set. */
    define {&mode} temp-table ttVersionReset no-undo
      field record_version_obj as decimal 
      field delete_record      as logical
      index pudx is unique primary
        record_version_obj
      .
    
    /* This table is used to contain the loaded version information for the duration of
       an import. For the duration of the load this means we have a list of records and 
       the current version that their data is at */
    define {&mode} temp-table ttImportVersion no-undo
        field record_version_obj        as decimal format "->>>>>>>>>>>>>>>>>9.999999999"
        field entity_mnemonic           as character 
        field key_field_value           as character format "X(70)"
        field version_number_seq        as decimal  format "->>>>>>>>>>>>>>>>>9.999999999"
        field version_date              as date 
        field version_time              as integer   format ">>>>9"
        field version_user              as character format "X(20)"
        field deletion_flag             as logical 
        field import_version_number_seq as decimal format "->>>>>>>>>>>>>>>>>9.999999999"
        field last_version_number_seq   as decimal format "->>>>>>>>>>>>>>>>>9.999999999"
        field secondary_key_value       as character format "X(70)"
        field oObjOnDB as decimal 
        index pudx is unique primary record_version_obj
      .
    
    /* This table is just a temporary working table that can be used to manipulate the 
       data that will be stored in ttImportVersion */
    define {&mode} temp-table ttImport no-undo like ttImportVersion.
    
    /* This table is used to receive the parameters for an import from the caller. */
    define {&mode} temp-table ttTableProps no-undo
      field cEntityMnemonic     as character
      field lOverWrite          as logical
      field lDeleteRelated      as logical
      field lKeepOwnSiteData    as logical
      index pudx is unique primary
        cEntityMnemonic
      .
    
    /* This table contains a list of records that may need to be deleted at the end
       of dataset import process. */
    define {&mode} temp-table ttDeleteList no-undo
      field cEntityMnemonic     as character
      field iKey                as integer
      field lHasObj             as logical
      field cObjFieldList       as character
      field cObjFieldValue      as character
      field cKeyFieldList       as character
      field cKeyFieldValue      as character
      field hTTBuffer           as handle
      field cDBTable            as character
      index pudx is unique primary
        cEntityMnemonic
        iKey
      .
    
    /* This temp-table is used to pass parameters into import deployment dataset */
    define {&mode} temp-table ttImportParam no-undo
      field cParam             as character
      field cValue             as character
      index dx is primary
        cParam
      .
    
    /* This temp-table is used to pass parameters into import deployment dataset */
    define {&mode} temp-table ttExportParam no-undo
      field cParam             as character
      field cValue             as character
      index dx is primary
        cParam
      .
    
    define {&mode} temp-table ttADOParam no-undo
      field cParam             as character
      field cValue             as character
      index dx is primary
        cParam
      .
    
    
    /* The following two temp-table are used for release versioning to build up the
       list of data that needs to be exported. */
    define {&mode} temp-table ttExportRecordSet  no-undo 
      field dataset_code        as character //like gsc_deploy_dataset.dataset_code
      field cKey                as character 
      field cFileName           as character 
      index udx is unique primary
        dataset_code
        cKey
      .
    
    define {&mode} temp-table ttExportDataset no-undo 
      field dataset_code        as character //like gsc_deploy_dataset.dataset_code
      field cFileName           as character 
      field source_code_data    as logical  // like gsc_deploy_dataset.source_code_data
      index pudx is unique primary
        dataset_code
      .
    
    define {&mode} temp-table ttPatchList no-undo
      field iSeq             as integer
      field cDatasetCode     as character
      field cPatchLevel      as character
      field cStage           as character
      field cFileType        as character
      field cFileName        as character
      field cDescription     as character
      field lRerunnable      as logical
      field lNewDB           as logical
      field lExistingDB      as logical
      field lUpdateMandatory as logical
      index pudx is primary unique
        cFileName
      index dxSeq
        iSeq
        cFileName
      index dxDSCode
        cDatasetCode
      .
    
    define {&mode} temp-table ttObjectList no-undo
      field cObjectName     as character
      field cObjectType     as character
      field lObjectInUse    as logical
      field hObjectHandle   as handle
      index pudx is unique primary
        hObjectHandle
      index dxSearch
        cObjectName
        cObjectType
        lObjectInUse
      .
    
    define {&mode} temp-table ttDummy no-undo
      field cTest as integer
      index pudx is unique primary
        cTest
      .
    
    define {&mode}  dataset dsAdo for
                    ttentitylist, 
                    ttRequiredRecord, 
                    ttTransaction,
                    ttAdoparam, 
                    ttDSAttribute,
                    ttTable, 
                    ttTableList,
                    ttTableProps, 
                    ttPatchlist,
                    ttObjectlist,
                    ttNode
         data-relation rel1 for  ttEntityList, ttTable relation-fields(cEntityMnemonic, cEntityMnemonic) nested         .
    
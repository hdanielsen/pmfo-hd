 
 /*------------------------------------------------------------------------
    File        : AdoModel
    Purpose     : 
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : 7/24/2024 
    Notes       : temp-tables and xml load copied from /af/app/gscddxmlp.p

  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.Model.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using Pmfo.Tools.Gui.Model.AppBuilderModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
using Pmfo.Tools.Gui.Model.AbstractSdoModel from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using OpenEdge.DataAdmin.Error.IllegalOperationError from propath.
using Pmfo.Util.StringUtil from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.DataAdmin.IField from propath.
using OpenEdge.DataAdmin.IFieldSet from propath.
using OpenEdge.DataAdmin.ITable from propath.
 
 

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.AdoModel abstract inherits AbstractSdoModel  : 
    &scop mode protected
    {Pmfo/Tools/AppBuilder/dynamicsdata.i}
    &undefine mode
    
    define property ghXMLHlpr as handle no-undo 
        get():
            if not valid-handle(ghXMLHlpr) then
            do:
                if search("af/app/afxmlhlprp.p") <> ? then  
                    run af/app/afxmlhlprp.p persistent set ghXMLHlpr.
                else 
                    undo, throw new IllegalOperationError("Load of ADO without dynamic source code in propath").    
            end.
            return ghXMLHlpr.             
        end.
        set.
    
    define protected property CalculatedFieldMethods as character no-undo get. set. 
    define protected property DbNames as character no-undo get. set. 
    
    define variable giRequestNo as integer.
    
    constructor public AdoModel (  ):
        super().
    end constructor.
    
    method public abstract void  buildStructFromDB(
      input        pcDatasetCode as character, 
      input-output piRequestNo   as integer, 
      output       piNoTables    as integer 
    
      ). 
    
    method override public logical Parse(pcfilename as char  ):
        define variable lok as logical no-undo.
        /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEFINE INPUT  PARAMETER pcFileName    AS CHARACTER  NO-UNDO.*/
/*  DEFINE OUTPUT PARAMETER piRequest     AS INTEGER    NO-UNDO.*/
/*  DEFINE OUTPUT PARAMETER phTables      AS HANDLE     NO-UNDO.*/
/*  DEFINE OUTPUT PARAMETER phTableList   AS HANDLE     NO-UNDO.*/
/*  DEFINE OUTPUT PARAMETER phTrans       AS HANDLE     NO-UNDO.*/
/*  DEFINE OUTPUT PARAMETER phEntity      AS HANDLE     NO-UNDO.*/

        define variable iRequest  as integer    no-undo.
        define variable mXMLDoc   as memptr     no-undo.
        define variable hXMLDoc   as handle     no-undo.
        define variable hRootNode as handle     no-undo.
        define variable hBuffer   as handle     no-undo.
        define variable hTable    as handle     no-undo.
        define variable iCount    as integer    no-undo.
        define variable cAssignlist as character no-undo.
          /*  This is here for debugging purposes to add the 
              internal temp tables so that they can be browsed too. 
          DEFINE VARIABLE cTableList AS CHARACTER  INITIAL
            "ttTable,ttRequiredRecord,ttEntityList,ttNode,ttIndexList,ttDSAttribute,ttReqHandle,ttImportVersion":U
            NO-UNDO.
                                                                    */
        set-size(mXMLDoc) = 0.
        
        parseXMLHeader (pcFileName, mXMLDoc, no, no, output iRequest).
       // piRequest = iRequest.
        
        set-size(mXMLDoc) = 0.
        
          /*
          DO iCount = 1 TO NUM-ENTRIES(cTableList):
            createTable(iRequest, ENTRY(iCount,cTableList)).
          END.
            */
        //  phTables    = buffer ttTable:handle.
        //  phTableList = buffer ttTableList:handle. 
       //   phTrans     = buffer ttTransaction:handle.
        //  phEntity    = buffer ttEntityList:handle.
        
          if return-value <> "":U then
              undo, throw new ApplicationError(subst("Load failed &1",return-value)).
          
          /* Load the XML into the datasets */
          loadDataSet (iRequest, no).
          if return-value <> "":U then
              undo, throw new ApplicationError(subst("Load failed &1",return-value)).
          
                
        define variable cname as character no-undo.
        cname = entry(num-entries(pcfilename,"~\"),pcfilename,"~\").
        cName = StringUtil:RemoveEntryFromList("ado", cname,".").
        Name = cName.
     //   message cname            subst("temp/ado&1.json",cname) 
                      
     //   view-as alert-box.
        
        dataset dsAdo:write-json("file",
                                 subst("temp/ado&1.json",cname),
                                 yes).
        //Name = 
       
        for each ttTableList: 
        
            if valid-handle(ttTablelist.htable) then
            do:
                ttTablelist.htable:write-json("file",
                                 subst("temp/ado_&1_&2.json",ttTablelist.htable:name,cname),
                                 yes).
        
                if ttTablelist.htable:name = "tt_ryc_attribute_value" then
                do:
                    
                    hBuffer            = ttTablelist.htable:default-buffer-handle.
               //    cAssinglist = 
                //    message GetPropertyValue(hBuffer,'AssignList')
                //    view-as alert-box.
                    do on error undo, throw:
                        BaseQuery          = GetPropertyValue(hBuffer,'BaseQuery').
                        
                        cAssignlist        = GetPropertyValue(hBuffer,'AssignList',true).
                        
                        DataLogicProcedure = GetPropertyValue(hBuffer,'DataLogicProcedure').
                        QueryTables = GetPropertyValue(hBuffer,'Tables').
                        
                        this-object:Table   = entry(1,QueryTables).
                                         // todo more than one db ?? unsuppported error?
                        DbNames = GetPropertyValue(hBuffer,'QueryBuilderDBNames',true).
                        this-object:Database = entry(1,DbNames).
                        
                        if this-object:Database = "" then 
                            this-object:Database = entry(1,GetPropertyValue(hBuffer,'QueryBuilderTableList',true),".").
                       
                       
                         
                        catch e as Progress.Lang.Error :
                            message "faield to partse " name skip e:Getmessage(1)
                            view-as alert-box.
                            return false.    
                        end catch.
                    end.
                    AssignList  = BuildAssignlist(QueryTables,cAssignlist).
                    
                    file-info:file-name = DataLogicProcedure.
                    DataLogicProcedure  = file-info:full-pathname.
                    
                    this-object:Fields = LoadFields(
                                             hBuffer, 
                                             replace(cassignlist,";",","),
                                             output DataFields, 
                                             output EnabledFields,
                                             output CalculatedFieldMethods
                                             ).
                     
                end.
            end.
            
        end.
        
        return true.
      
    end method.    
    
    method protected character BuildAssignlist(pcBuffers as character, pcAssignlist as character):
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable cBufferAssigns as character no-undo.
        define variable cBuffer as character no-undo.
        define variable cAssignlist as character no-undo.
        define variable cField as character no-undo.
        define variable cDbField as character no-undo.
        do i = 1 to num-entries(pcAssignlist,";"):
            cBufferAssigns = entry(i,pcAssignList,";").
            cBuffer = entry(i,pcBuffers).
            do j = 1 to num-entries(cBufferAssigns) by 2:
                cField   = entry(j,cBufferAssigns).
                cDbField = entry(j + 1,cBufferAssigns).
                cAssignList = StringUtil:AppendEntry(cAssignList,subst("&1,&2.&3",cField,cBuffer,cDbField)).
                
            end.     
        end.    
        return cAssignlist. 
    end method.     
    
    method protected ISet LoadFields (
        phBuffer as handle,
        pcAssignlist as character,
        output pcDataFields as character, 
        output pcEnabledFields as character,
        output pcCalculatedFieldMethods as character
    ):
        
        define variable ofields    as ISet no-undo.
        define variable cDatabases as character extent no-undo.
        define variable iTblNum    as integer no-undo.
        define variable oField     as FieldModel no-undo.
        define variable iFld       as integer no-undo.
        define variable cDatatypes as character no-undo.
      
        define variable cDataColumnsByTable as character no-undo.
        define variable cUpdatableDataColumnsByTable as character no-undo.
        define variable cTableFields as character no-undo.
        define variable cUpdatableTableFields as character no-undo.
        define variable iTable as integer no-undo.
        define variable lIsCalc as logical no-undo.
        define variable lHasCalc as logical no-undo.
        oFields = new Set().
       
        pcDataFields = GetPropertyValue(phBuffer,'DataColumns').
         
        cDataColumnsByTable = GetPropertyValue(phBuffer,'DataColumnsByTable').
        cUpdatableDataColumnsByTable = GetPropertyValue(phBuffer,'UpdatableColumnsByTable').
        
        lHasCalc =  GetPropertyValue(phBuffer,'CalcFieldList',true) > "".
  
        if lHasCalc then 
            cDataTypes = GetPropertyValue(phBuffer,'QueryBuilderFieldDataTypes'). 
        do iTable = 1 to num-entries(QueryTables) + (if lHasCalc then 1 else 0):
            lIsCalc = iTable gt num-entries(QueryTables).
            cTableFields          = entry(iTable,cDataColumnsByTable,";").
            cUpdatableTableFields = if lHasCalc = false or num-entries(cUpdatableDataColumnsByTable,";") >= iTable
                                    then entry(iTable,cUpdatableDataColumnsByTable,";")
                                    else "".
            do iFld = 1 to num-entries(cTableFields):
                oField = new FieldModel(). 
                oField:inSDO = true.
                oFields:add(oField).
                oField:name        = entry(iFld,cTableFields).
                oField:iscalc      = lIscalc 
                    // querybuilder data types for db fields are not correct 
                    // will update from db table in ResourceModel  
                     
                    . 
                 
                if lookup(oField:name,cUpdatableTableFields) > 0  then 
                    pcEnabledFields = StringUtil:AppendEntry(pcEnabledFields, oField:name).                         
                
                if oField:isCalc = false then
                do:
                    assign
                        ofield:sourcetable =  entry(iTable,QueryTables)                     
                        ofield:sourceDb    = if DbNames > "" then entry(iFld,DbNames)
                                             else this-object:Database
                        ofield:sourceName  = StringUtil:GetUsename(oField:name,pcAssignList)                     
                        oField:isJoin      = ofield:sourcetable <> this-object:Table 
                         .                        
                end.
                else do: 
                    // for calc fields we can use the datatypes 
                    oField:Datatype = lc(entry(lookup(oField:name,pcDataFields),cDataTypes)).
                    pcCalculatedFieldMethods = StringUtil:AppendEntry(pcEnabledFields,subst("calculate&1",oField:name)).
                end.    
            end.
        end.
        return oFields.
    end method.    
    
    method public void UpdateDataFieldsFromDataAdmin(oTable as ITable):
        define variable oIter       as IIterator no-undo.
        define variable oField      as FieldModel  no-undo.
        define variable oDbField    as IField   no-undo.
        define variable odbFieldset as IFieldSet no-undo.
        define variable cAssignName as character no-undo.
        oDbfieldSet = oTable:Fields.
        oIter = this-object:Fields:Iterator().
        do while oIter:HasNext():
            oField = cast(oIter:Next(),FieldModel).
            if oField:sourceTable = oTable:name and oField:IsCalc = false then
            do:
                oDBField = oDbfieldSet:find(oField:Sourcename). 
                
                if valid-object(oDbField) then 
                    oField:dataType    = oDbField:DataType.
                    
            end.
        end.      
            
    end method.    
    
    method protected character GetPropertyValue(phBuffer as handle, pcName as character) :
        return GetPropertyValue(phBuffer,pcname,false).
    end method.
    
    method protected character GetPropertyValue(phBuffer as handle, pcName as character, plNoerror as logical) :
        //define variable hQuery as handle no-undo.
        phBuffer:buffer-release.
        phBuffer:find-unique (subst("where attribute_label = '&1'",pcname)).
        return phBuffer::character_value.
        
        
        /**
        create query hQuery.
        hQuery:add-buffer(phBuffer).
        hQuery:query-prepare(subst("for each &1",phBuffer:name)).
        hQuery:query-open().
        hQuery:get-first.
        do while phBuffer:avail:
            //case phBuffer::attribute-value:
            //    when "Tables" then
                do:
                end.
            end.    
            hQuery:get-next.
            
        end.
        finally:
            if valid-object(hQuery) then
               delete object hQuery.
        end.
        */    
        catch e as Progress.Lang.Error :
            if plNoError then 
                return "".  
            undo, throw new ApplicationError(subst("Attribute Label '&1' not found in '&2' for '&3'",
                                                    pcname,
                                                    phBuffer:name,
                                                    name
                                                   )
                                             ).    
        end catch.
    end.    
    
    method protected logical parseXMLHeader (
        input pcFileName           as character,
        input pmXMLDoc             as memptr,
        input plWriteHeaderData    as logical,
        input plCreateErrHeader    as logical,
        output piRequestNo         as integer 
     
     ):
    /*------------------------------------------------------------------------------
      Purpose:     This procedure loads the XML header and sets up the attributes
                   about the header.
      Parameters:  
        pcFileName        - The name of the XML file to be loaded. If set to "<MEMPTR>",
                            the document is loaded from pmXMLDoc.
        pmXMLDoc          - A memptr containing the XML doc to be loaded. It is only
                            used if pcFileName = "<MEMPTR>". Both are passed to the
                            loadXMLDoc function which takes care of loading the 
                            physical XML doc.
        plWriteHeaderData - Indicates whether or not the header information should
                            be written to the local database. This only applies if 
                            the XML file contains the relevant header information.
                            Code that simply wants to examine the contents of the
                            XML file will probably not want to load the header.  
                            
        plCreateErrHeader - Indicates whether an error file header should be created.
                            If set to yes, the routine will parse the XML file and 
                            create a new XML document. This document will contain (at 
                            the end of this procedure) the header information, the dataset 
                            node and the dataset_header node which will be copied from 
                            the incoming document. This facilitates the creation
                            of the error XML document that will contain all the
                            dataset transactions that fail to load during an 
                            import.
                            If set to no, the error file is not created.                                   
        OUTPUT 
        piRequestNo       - The request number associated with all the attributes
                            of this request. All temp-tables, attributes and
                            handles are related via this request number.           
      Notes:  
        5/5/2001  - As this code stands right now, there is no support for the 
                    header information being stored in the XML file. All the 
                    header information needs to be in the database.     
    ------------------------------------------------------------------------------*/
      
        define variable hXDoc         as handle   no-undo.
        define variable hRootNode     as handle   no-undo.
        define variable hChildNode    as handle   no-undo.
        define variable lSuccess      as logical  no-undo.
        define variable lFullHeader   as logical    no-undo.
        define variable lValidHeader  as logical    no-undo.
        define variable iCount        as integer  no-undo.
        define variable cRetVal       as character  no-undo.
        define variable cAtt          as character  no-undo.
        define variable cAttVal       as character  no-undo.
        define variable iRequestNo    as integer    no-undo.
        define variable iNoTables     as integer    no-undo.
        define variable cDataSetCode  as character  no-undo.
        define variable hErrDoc       as handle     no-undo.
        define variable hErrRoot      as handle     no-undo.
        define variable hErrDSH       as handle     no-undo.
      
        /* Load the XML document */
        hXDoc = dynamic-function("loadXMLDoc":U in ghXMLHlpr,
                                 pcFileName,
                                 pmXMLDoc,
                               output cRetVal).
    
        /* The return value is already set. Just return if the handle is invalid */
        if not valid-handle(hXDoc) then
        do:
            if cRetVal = "EMPTY FILE":U then
                return true.
            else
               undo, throw new ApplicationError(subst("Load failed: &1",cRetval)).
        end.
        cRetVal = "":U.
    
        /* Get a new request no */
        giRequestNo = giRequestNo + 1.
        iRequestNo = giRequestNo.
      
        /* Make sure that all previous calls for this request number are gone */
        deleteTempTables(iRequestNo).
        deleteAttributes(iRequestNo).
        deleteReqHandles(iRequestNo).
    
        /* Create two node references */
        create x-noderef hRootNode.
    
       /* Set the root node */
        lSuccess = hXDoc:get-document-element(hRootNode).
    
        /* If we're not successful we have an invalid XML file */
        if not lSuccess then
        do on error undo, throw:
            undo, throw new ApplicationError(subst("Cannot load ADO &1",pcfilename)).
            finally:
         // cRetVal = {af/sup2/aferrortxt.i 'AF' '122' '?' '?' 'root' pcFileName}.
                delete object hRootNode.
                hRootNode = ?.
                delete object hXDoc.
                hXDoc = ?.
            end.
           // return cRetVal.
        end.
    
       /* Get the elements for the root node */
        do-blk:
        do iCount = 1 to num-entries(hRootNode:attribute-names):
            cAtt = entry(iCount, hRootNode:attribute-names).
            if num-entries(cAtt,"_":U) > 1 then
                next do-blk.
            cAttVal = hRootNode:get-attribute(cAtt).
            setAttribute(iRequestNo, cAtt, cAttVal). 
            if cAtt = "EmptyDataset":U and
            cAttVal = "YES":U then
                return true.
        end.
    
        /* Create a Child Node ref */
        create x-noderef hChildNode.
    
        /* Iterate through the root node's children */
        repeat iCount = 1 to hRootNode:num-children:
        
            /* Set the current Child Node */
            lSuccess = hRootNode:get-child(hChildNode,iCount).
        
            if not lSuccess then
                next.
        
            /* If the child node is blank, skip it */
            if hChildNode:subtype = "TEXT":U and
                hChildNode:node-value = CHR(10) then
                    next.
        
            /* If the name of this node is "dataset_header" , we'll process this node */  
            if hChildNode:name = "dataset_header":U then
            do:
                if plCreateErrHeader then
                do:
                    createTopLevelNodes(output hErrDoc, output hErrRoot).
                    create x-noderef hErrDSH.
                    hErrDoc:import-node(hErrDSH, hChildNode, true).
                    hErrRoot:append-child(hErrDSH).
                    delete object hErrDSH.
                    hErrDSH = ?.
                end.
              
                  /* Get all the attribute values for this node */
                do iCount = 1 to num-entries(hChildNode:attribute-names):
                    cAtt = entry(iCount, hChildNode:attribute-names).
                    cAttVal = hChildNode:get-attribute(cAtt).
                    setAttribute(iRequestNo, cAtt, cAttVal). 
                end.
            
                  /* Set the full header and data set code attributes */
                lFullHeader  = getAttribute(iRequestNo, "FullHeader":U) = "YES":U.
                cDataSetCode = getAttribute(iRequestNo, "DatasetCode":U).
            
                  /* If the header or dataset have not been specified, there's a problem */
                if lFullHeader = ? or 
                    cDatasetCode = ? then
                do:
                    undo, throw new ApplicationError(subst("Cannot load header of ADO &1",pcfilename)).
                   //  cRetVal = {af/sup2/aferrortxt.i 'AF' '123' '?' '?' pcFileName 'header'}.
                   
                end.
                else
                    lSuccess = yes.
                
                if not lSuccess then
                    leave.
        
                /* Let's see if the root node has any data versioning info. This has to happen
                   here because we needed to set the date format before we tried this. 
                    This is a fix for Issue 3350 */
              //  setAttribute(iRequestNo,"DatasetVersionInfo":U, string(readRecordVersionAttr(iRequestNo, hRootNode))). 
        
                if lFullHeader then
                do:
                    /* At this stage, I'm ignoring the header definitions section. I'll
                        put this in later. At this point, we always build the temp-tables
                        from the database schema */
                     buildStructFromDB(cDatasetCode, input-output iRequestNo, output iNoTables).
                end.
                else
                do:
                    buildStructFromDB(cDatasetCode, input-output iRequestNo, output iNoTables).
                end.
                
             // {af/sup2/afcheckerr.i 
               //  &no-return = YES}
               
               // else do: 
                    lValidHeader = yes.
                    setAttribute(iRequestNo, "NoTables":U, string(iNoTables)).
               // end.
                leave.
            end.
          
        end.
        
        if lValidHeader then
        do:
            registerObject(iRequestNo, "hRootNode":U, hRootNode).
            registerObject(iRequestNo, "hXDoc":U, hXDoc).
            if valid-handle(hErrRoot) then
              registerObject(iRequestNo, "hErrNode":U, hErrRoot).
            if valid-handle(hErrDoc) then
              registerObject(iRequestNo, "hErrDoc":U, hErrDoc).
            setAttribute(iRequestNo, "NoErrors":U, "0":U).
            piRequestNo = iRequestNo.
        end.
        else
        do:
            deleteTempTables(iRequestNo).
            deleteAttributes(iRequestNo).
            deleteReqHandles(iRequestNo).
            if valid-handle(hErrRoot) then
              delete object hErrRoot.
            if valid-handle(hErrDoc) then
              delete object hErrDoc.
            delete object hRootNode.
            hRootNode = ?.
            delete object hXDoc.
            hXDoc = ?.
        end.
        
          /* Delete the objects */
        delete object hChildNode.
        hChildNode = ?.
        
        //if cRetVal <> "":U then
          //  return cRetVal.
  
    end method.
    
    method protected void createTopLevelNodes(
        output phXMLDoc   as handle, 
        output phRootNode as handle  
    
     ) :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
      create x-document phXMLDoc.
    
      phXMLDoc:encoding = "utf-8":U.
    
      /* Create a table_def node */
      phRootNode = dynamic-function("createElementNode":U in ghXMLHlpr,
                                   phXMLDoc, 
                                   "dataset":U).
    
    
    end method.
    
    method protected void loadDataSet
        (input  piRequestNo   as integer,
         input  plImportData  as logical) :
    /*------------------------------------------------------------------------------
      Purpose:     This procedure reads through the dataset nodes and loads them 
                   into the temp tables. If the lImport flag is set, it will take 
                   the contents of the temp-tables and write them to the databases.
      Parameters:
        piRequestNo   - The request no to load datasets for.
        plImportData  - A flag to indicate whether the data should physically
                        be written to the database. If set to yes, the dataset
                        will be written to the database and an XML exception log 
                        file produced if need be.
      Notes:       
    ------------------------------------------------------------------------------*/
     
    
      define variable hXDoc         as handle   no-undo.
      define variable hRootNode     as handle   no-undo.
      define variable hChildNode    as handle   no-undo.
      define variable lFailed       as logical    no-undo.
      define variable lSuccess      as logical    no-undo.
      define variable cRetVal       as character  no-undo.
      
      define variable iCount        as integer    no-undo.
    
      hXDoc     = getObjectHandle(piRequestNo, "hXDoc":U).
      /* Sanity check. We should not get the unknown value here, but if we do
         we need to drop out */
      if not valid-handle(hXDoc) then
      do:
          undo, throw new ApplicationError("Invalid Document handle encountered during load of ADO").
       //   cRetVal = {af/sup2/aferrortxt.i 'AF' '123' '?' '?' "''" "'Document Handle'"}.
       //  return cRetVal.
      end.
       
         
      hRootNode = getObjectHandle(piRequestNo, "hRootNode":U).
      /* Sanity check. We should not get the unknown value here, but if we do
         we need to drop out */
      if not valid-handle(hRootNode) then
      do:
          undo, throw new ApplicationError("Invalid Root node handle encountered during load of ADO").
          
       //  cRetVal = {af/sup2/aferrortxt.i 'AF' '123' '?' '?' "''" "'Root Node Handle'"}.
       //  return cRetVal.
      end.
       
      /* Create a Child Node ref */
      create x-noderef hChildNode.
      
      /* Iterate through the root node's children */
      repeat iCount = 1 to hRootNode:num-children:
        
        /* Set the current Child Node */
        hRootNode:get-child(hChildNode,iCount).
        
        /* If the child node is not the dataset records child node, skip it */
        if hChildNode:subtype = "TEXT":U or
           hChildNode:name <> "dataset_records":U then
          next.
        
        /* Now we need to pass this node on to the transaction processor */
        loadTransactionNodes (piRequestNo, hChildNode, plImportData).
        if return-value <> "":U then
            undo, throw new ApplicationError(subst("Failed to load transaction node: &1",return-value)).
         
      end. /* REPEAT iCount = 1  */
      
      delete object hChildNode.
      hChildNode = ?.
      
    end method.
    
    method protected void writeTempTableData (
      input piRequestNo as integer,
      input piNodeLevel as integer,
      input piTableNo   as integer,
      input piTransNo   as integer
        
    ):
    /*------------------------------------------------------------------------------
      Purpose:     Takes the date from the ttNode table and writes it to the
                   appropriate TEMP-TABLE.
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
      
      define variable cMessage            as character  no-undo.
      define variable hTempTable          as handle     no-undo.
      define variable hTTBuff             as handle     no-undo.
      define variable hWriteBuff          as handle     no-undo.
      define variable hField              as handle     no-undo.
      define variable iCount              as integer    no-undo.
    
      define buffer bttNode        for ttNode.
      define buffer bttTableList   for ttTableList.
      define buffer bttTransaction for ttTransaction.
    
      /* First we have to find the buffer to for the temp-table that this record 
         belongs to. */
    
      find first bttTableList no-lock
        where bttTableList.iRequestNo = piRequestNo
          and bttTableList.iTableNo    = piTableNo no-error.
      if not available(bttTableList) then
      do:
         undo, throw new ApplicationError(subst("Failed to load table no: &1",piTableno)).
      end.
    
      /* Obtain the handle to the temp-table. */
      hTempTable = bttTableList.hTable.
      if valid-handle(hTable) and 
         hTable:type = "TEMP-TABLE":U then
        hTTBuff = hTable:default-buffer-handle. /* Get a handle to the TT Buffer */
    
      /* If we don't have a handle to a valid buffer at this point, there is a problem */
      if not valid-handle(hTTBuff) then
      do:
         undo, throw new ApplicationError(subst("Failed to load &1  &3",bttTableList.cDBName,cTablename)).
      end.

      /* Now we need to create an alternate buffer for this temp-table */
      obtainPoolObject /* IN ghObjectPool */
        ("BUFFER":U, 
         string(hTTBuff), 
         output hWriteBuff).

      /* Write the data into the temp-table */
      do transaction:
        /* Before we can write the data, we need to have available
           in the buffer, the appropriate record that will be updated.
           The following call will try and find a record which will be
           returned in hWriteBuff. */
        
        obtainTempTableRec(piRequestNo,
                           piNodeLevel,
                           piTableNo,
                           hWriteBuff).
        
        /* If we didn't find a record for the temp-table, create one */
        if not hWriteBuff:available then
          hWriteBuff:buffer-create().
    
        /* Now iterate through each of the nodes and assign the field 
           value to the temp-table record. */
        for each bttNode no-lock
          where bttNode.iRequestNo   = piRequestNo
            and bttNode.iLevelNo     = piNodeLevel:
          /* Obtain the handle to the buffer field. */
          hField = hWriteBuff:buffer-field(bttNode.cNode) no-error.
    
          /* If the field handle is valid, convert the data to the 
             appropriate data type and assign it to the correct node. */
    
    
          if valid-handle(hField) then
          do:
            if hField:data-type <> "CHARACTER":U and
               bttNode.cValue = "?":U or
               bttNode.cValue = ? then
            do:
              if hField:extent > 0 then
              do iCount = 1 to hField:extent:
                hField:buffer-value[iCount] = ?.
              end.
              else
                hField:buffer-value = ?.
            end.
            else
              setFieldValue(piRequestNo, hField, bttNode.cValue, bttNode.iExtentIndex).
          end.
        end.
    
        if piNodeLevel = 1 then 
        do:
          create bttTransaction.
          assign
            bttTransaction.iRequestNo = piRequestNo
            bttTransaction.iTransNo   = piTransNo
            bttTransaction.rRowid     = hWriteBuff:rowid
          .
        end.
    
        /* Release the record so it gets written to the table */
        hWriteBuff:buffer-release().
    
      end.
    
      /* Delete the buffer */
      releasePoolObject /* IN ghObjectPool */
        (hWriteBuff).
      hWriteBuff = ?.
    
      /* Delete the node records at this level from the
         temp-table. */
      for each bttNode    
        where bttNode.iRequestNo   = piRequestNo
          and bttNode.iLevelNo     = piNodeLevel:
        delete bttNode.
      end.
    
      return.
    end method.
    
    method protected logical setFieldValue 
      ( input piRequestNo    as integer,
        input phField        as handle,
        input pcFieldValue   as character,
        input piExtentIndex  as integer) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
      define variable cCurrDateFormat     as character  no-undo.
      define variable cDatasetDateFormat  as character  no-undo.
      define variable cCurrNumFormat      as character  no-undo.
      define variable cDatasetNumFormat   as character  no-undo.
      define variable cCurrNumSep         as character  no-undo.
      define variable cDatasetNumSep      as character  no-undo.
      define variable cCurrNumDec         as character  no-undo.
      define variable cDatasetNumDec      as character  no-undo.
      define variable cValue              as character  no-undo.
     
      case phField:data-type:
        when "INTEGER":U then
        do:
          if piExtentIndex > 0 then
          do:
            if piExtentIndex < phField:extent then
             phField:buffer-value[piExtentIndex] = integer(pcFieldValue).
          end.
          else
            phField:buffer-value = integer(pcFieldValue).
        end.
        when "DECIMAL":U then
        do:
          /* With a decimal, we need to swap the numeric format so that it
             is the same as it was in the incoming dataset */
          cCurrNumSep = session:numeric-separator.
          cDatasetNumSep = getAttribute(piRequestNo,"NumericSeparator":U).
          cCurrNumDec = session:numeric-decimal-point.
          cDatasetNumDec = getAttribute(piRequestNo,"NumericDecimal":U).
          if cDatasetNumSep <> ? and
             cDatasetNumSep <> "":U and
             cDatasetNumDec <> ? and
             cDatasetNumDec <> "":U then
            session:set-numeric-format(cDatasetNumSep, cDatasetNumDec).
          if piExtentIndex > 0 then
          do:
            if piExtentIndex < phField:extent then
              phField:buffer-value[piExtentIndex] = decimal(pcFieldValue).
          end.
          else
            phField:buffer-value = decimal(pcFieldValue).
          session:set-numeric-format(cCurrNumSep, cCurrNumDec).
        end.
        when "DATE":U or
        when "DATETIME":U or
        when "DATETIME-TZ":U then
        do:
          /* With a date, we need to swap the date format so that it
             is the same as it was in the incoming dataset */
          cCurrDateFormat = session:date-format.
          cDatasetDateFormat = getAttribute(piRequestNo,"DateFormat":U).
          if cDatasetDateFormat <> ? and
             cDatasetDateFormat <> "":U then
            session:date-format = cDatasetDateFormat.
          if piExtentIndex > 0 then
          do:
            if piExtentIndex < phField:extent then
            do:
              case phField:data-type:
                when "DATE":U then
                  phField:buffer-value[piExtentIndex] = date(pcFieldValue).
                when "DATETIME":U then
                  phField:buffer-value[piExtentIndex] = datetime(pcFieldValue).
                when "DATETIME-TZ":U then
                  phField:buffer-value[piExtentIndex] = datetime-tz(pcFieldValue).
              end.
            end.
          end.
          else
          do:
            case phField:data-type:
              when "DATE":U then
                phField:buffer-value = date(pcFieldValue).
              when "DATETIME":U then
                phField:buffer-value = datetime(pcFieldValue).
              when "DATETIME-TZ":U then
                phField:buffer-value = datetime-tz(pcFieldValue).
            end.
          end.
          session:date-format = cCurrDateFormat.
        end.
        when "LOGICAL":U then
        do:
          if piExtentIndex > 0 then
          do:
            if piExtentIndex < phField:extent then
              phField:buffer-value[piExtentIndex] = trim(pcFieldValue) = "YES":U or TRIM(pcFieldValue) = "TRUE":U.
          end.
          else
            phField:buffer-value = trim(pcFieldValue) = "YES":U or TRIM(pcFieldValue) = "TRUE":U.
        end.
        when "CHARACTER":U then
        do:
          cValue = pcFieldValue.
          if cValue = ? or 
             cValue = "?":U then
            cValue = "?":U.
          else
            cValue = replaceCtrlChar(cValue,no).
           /* IZ 4064
          setFieldLiteral(phField, YES). /* Make sure we handle ? properly */
          */
            if piExtentIndex > 0 then
            do:
              if piExtentIndex < phField:extent then
                phField:buffer-value[piExtentIndex] = cValue.
            end.
            else
              phField:buffer-value = cValue.
          /*
          setFieldLiteral(phField, NO).
          */
        end.
      end.
    
      return false.   /* Function return value. */
    
    end method.
    
    method protected character replaceCtrlChar 
      ( input pcString as character,
        input plRemove as logical ) :
    /*------------------------------------------------------------------------------
      Purpose:     This procedure replaces control characters 1 to 8 in any string 
                   with the character constant #CHR(x)# where x is the ASCII value 
                   of the control character.
      Parameters: 
        pcString - The source string to replace the characters in
        plRemove - A logical variable which indicates if we are to remove or replace 
                   the control characters. If set to yes, the control characters
                   will be removed. If set to yes, they will be replaced.
                   
        Notes:  Certain control characters cannot be written to XML files as the 
                XML parser does not support them. This procedure does not replace 
                all unsupported characters. It only replaces the ones that are 
                used by the framework (CHR(1) to CHR(8)). 
    
    ------------------------------------------------------------------------------*/
    
      define variable iCount     as integer    no-undo.
    
      if plRemove then
      do iCount = 1 to 8:
        pcString = replace(pcString, chr(iCount), "#CHR(":U + STRING(iCount) + ")#":U).
      end.
      else
      do iCount = 1 to 8:
        pcString = replace(pcString, "#CHR(":U + STRING(iCount) + ")#":U, chr(iCount)).
      end.
    
      return pcString.   /* Function return value. */
    
    end method.
    
    
    method protected logical obtainTempTableRec 
      ( input piRequestNo as integer,
        input piNodeLevel as integer,
        input piTableNo   as integer,
        input phWriteBuff as handle ) :
    /*------------------------------------------------------------------------------
      Purpose:  Attempts to find a record in phWriteBuff that has key values that 
                are set in the node table.
        Notes:  
    ------------------------------------------------------------------------------*/
      define variable iCount        as integer    no-undo.
      define variable cIndexInfo    as character  no-undo.
      define variable cIndexField   as character  no-undo.
      define variable cWhereClause  as character  no-undo.
      define variable hField        as handle     no-undo.
      define variable cPhrase       as character  no-undo.
      define variable hQuery        as handle     no-undo.
      define variable lAns          as logical    no-undo.
    
      define buffer bttNode for ttNode.
    
      /* Find the primary index on this buffer */
      repeat:
        iCount = iCount + 1.
        cIndexInfo = phWriteBuff:index-information(iCount).
        if cIndexInfo = ? or
           cIndexInfo = "":U then 
          leave.
    
        if entry(3,cIndexInfo) = "1":U then
          leave.
      
      end.
    
      if cIndexInfo = ? or 
         cIndexInfo = "":U then
        return false.
    
      /* Loop through all the fields in the index, and build up a where clause
         with their values from the ttNode table */
      repeat iCount = 5 to num-entries(cIndexInfo) by 2:
        cIndexField = entry(iCount,cIndexInfo).
    
        /* Get a handle to the buffer field */
        hField = phWriteBuff:buffer-field(cIndexField) no-error.
    
        if not valid-handle(hField) then
        do:
           
          //message "Invalid field handle - " cIndexField
            //view-as alert-box info buttons ok.
          cWhereClause = ?.
          leave.
        end.
    
        /* Find the node record that has the value of this field */
        find first bttNode no-lock
          where bttNode.iRequestNo = piRequestNo
            and bttNode.iLevelNo   = piNodeLevel
            and bttNode.iTableNo   = piTableNo
            and bttNode.cNode      = cIndexField no-error.
        if not available(bttNode) then
        do:
          cWhereClause = ?.
          leave.
        end.
    
        /* Build up a phrase for this field. */
        cPhrase = cIndexField + " = ":U + convDTForWhere(hField:data-type,
                                                         bttNode.cValue,
                                                         piRequestNo,
                                                         "D":U).
        
        if cWhereClause = "":U then
          cWhereClause = "WHERE " + cPhrase.
        else
          cWhereClause = " AND " + cPhrase.
          
      end.
    
      if cWhereClause = ? then
        return false.
    
      phWriteBuff:find-first(cWhereClause) no-error.
      error-status:error = no.
    
      return phWriteBuff:available.   /* Function return value. */

    end method.
    
    method protected character convDTForWhere
      ( input pcDataType    as character,
        input pcValue       as character,
        input piRequestNo   as integer,
        input pcDataFormat  as character ) :
    /*------------------------------------------------------------------------------
      Purpose:   Converts the a string value to the appropriate expression in 
                 a where clause for a dynamic query based on the datatype passed 
                 in.
                 
                 pcDataFormat is a code that can be one of:
                 D - Dataset format  (Whatever is in the dataset)
                 S - Standard format (MDY and ,.)
                 Anything else will result in standard date format being used.
        Notes:  
    ------------------------------------------------------------------------------*/
      define variable cCurrDateFormat     as character  no-undo.
      define variable cUseDateFormat  as character  no-undo.
      define variable cCurrNumSep         as character  no-undo.
      define variable cUseNumSep      as character  no-undo.
      define variable cCurrNumDec         as character  no-undo.
      define variable cUseNumDec      as character  no-undo.
      define variable cRetVal             as character  no-undo.
      define variable deValue             as decimal    no-undo.
      define variable daValue             as date       no-undo.
    
      case pcDataFormat:
        when "D":U then
        do:
          cUseNumDec     = getAttribute(piRequestNo,"NumericDecimal":U).
          cUseNumSep     = getAttribute(piRequestNo,"NumericSeparator":U).
          cUseDateFormat = getAttribute(piRequestNo,"DateFormat":U).
        end.
        when "S":U then
        do:
          cUseNumDec     = ".":U.
          cUseNumSep     = ",":U.
          cUseDateFormat = "mdy":U.
        end.
      end case.
    
      case pcDataType:
        when "CHARACTER":U then
          cRetVal = "'":U + pcValue + "'":U.
        when "DECIMAL":U then
        do:
          /* With a decimal, we may need to change the numeric format */
          cCurrNumSep = session:numeric-separator.
          cCurrNumDec = session:numeric-decimal-point.
          if cUseNumSep <> ? and
             cUseNumSep <> "":U and
             cUseNumDec <> ? and
             cUseNumDec <> "":U then
            session:set-numeric-format(cUseNumSep, cUseNumDec).
          deValue = decimal(pcValue) no-error.
          session:set-numeric-format(cCurrNumSep, cCurrNumDec).
          if error-status:error then
          do:
            error-status:error = no.
            cRetVal = ?.
          end.
          cRetVal = quoter(deValue).
        end.
        when "DATE":U then
        do:
          /* With a date, we need to swap the date format so that it
             is the same as it was in the incoming dataset */
          cCurrDateFormat = session:date-format.
          cUseDateFormat = getAttribute(piRequestNo,"DateFormat":U).
          if cUseDateFormat <> ? and
             cUseDateFormat <> "":U then
            session:date-format = cUseDateFormat.
          daValue = date(pcValue) no-error.
          session:date-format = cCurrDateFormat.
          if error-status:error then
          do:
            error-status:error = no.
            cRetVal = ?.
          end.
          cRetVal = quoter(daValue).
        end.
    
        when "INTEGER":U or
        when "LOGICAL":U then
        do:
          cRetVal = trim(pcValue) no-error.
          if error-status:error then  
          do:
            error-status:error = no.
            cRetVal = ?.
          end.
        end.
      end.
    
      return cRetVal.   /* Function return value. */
    
    end method.
    
    
    method protected void obtainPoolObject(
          input pcObjectType    as character,
          input pcObjectName    as character,
          output phObjectHandle  as handle 
    ) :
    /*------------------------------------------------------------------------------
      Purpose:     This procedure returns an object handle to the caller of the type
                   and name (if provided) provided as inpt parameters.
                   If there are no available objects of the specified type and
                   name, a new one is created.
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
      
      define variable hObjectHandle           as handle     no-undo.
      define variable hWorkHandle             as handle     no-undo.
    
      /* If the object type provided is not supported, return ? in the handle */
      if not can-do("BUFFER,TEMP-TABLE,QUERY,X-NODEREF":U,pcObjectType) then
      do:
        phObjectHandle = ?.
        return.
      end.
      
      /* See if there is an object in the object pool with this name and type. */
      find first ttObjectList 
        where ttObjectList.cObjectName = pcObjectName
          and ttObjectList.cObjectType = pcObjectType
          and not ttObjectList.lObjectInUse
        no-error.
    
      /* If we don't have an object in the pool, or the handle is invalid,
         we need to create it at this point */
      if not available(ttObjectList) or
         not VALID-HANDLE(ttObjectList.hObjectHandle) then
      do:
        case pcObjectType:
          
          when "TEMP-TABLE":U then
            create temp-table hObjectHandle.
    
          when "BUFFER":U then
          do:
            /* When a buffer is being created, we could get a handle that we have to base
               the new buffer on. */
            assign
              hWorkHandle = widget-handle(pcObjectName)
              NO-ERROR.
            if error-status:error then
              error-status:error = no.
            if valid-handle(hWorkHandle) then
            do:
              hObjectHandle = ?.
              case hWorkHandle:type:
                when "BUFFER":U then
                  /* If the handle is a BUFFER handle, create it based on that. */
                  create buffer hObjectHandle for table hWorkHandle no-error.
                when "TEMP-TABLE":U then
                  /* If the handle is a TEMP-TABLE handle, create it based on its buffer handle */
                  create buffer hObjectHandle for table hWorkHandle:default-buffer-handle no-error.
                otherwise
                  hObjectHandle = ?.
              end case.
            end.
            else
              create buffer hObjectHandle for table pcObjectName no-error.
            if error-status:error then
              error-status:error = no.
          end.
          
          when "QUERY":U then
            create query hObjectHandle.
          
          when "X-NODEREF":U then
            create x-noderef hObjectHandle.
        end case.
        if hObjectHandle <> ? then
        do:
          if not available(ttObjectList) then
          do:
            create ttObjectList.
            assign
              ttObjectList.cObjectName   = pcObjectName
              ttObjectList.cObjectType   = pcObjectType
            .
          end.
          assign
            ttObjectList.hObjectHandle = hObjectHandle
          .
        end.
      end.
      else
        hObjectHandle = ttObjectList.hObjectHandle.
    
      if hObjectHandle <> ? then
        assign
          ttObjectList.lObjectInUse  = true
        .
      assign
        phObjectHandle              = hObjectHandle
      .
    
    
    end method.
    
    method protected void releasePoolObject(
          input phObjectHandle   as handle
          
            ) :
        /*------------------------------------------------------------------------------
          Purpose:     Frees up an object handle when it is no longer in use.
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
          
          /* See if there is an object in the object pool with this name and type. */
          find first ttObjectList 
            where ttObjectList.hObjectHandle = phObjectHandle
            no-error.
        
          /* If we don't have an object in the pool, or the handle is invalid,
             we need to create it at this point */
          if available(ttObjectList) then
          do:
            if not valid-handle(phObjectHandle) then
              delete ttObjectList.
            else
            do:
              case ttObjectList.cObjectType:
        
                when "TEMP-TABLE":U then
                  phObjectHandle:default-buffer-handle:EMPTY-TEMP-TABLE().
        
                when "QUERY":U then
                do:
                  if phObjectHandle:is-open then
                    phObjectHandle:query-close().
                  phObjectHandle:set-buffers(buffer ttDummy:HANDLE).
                end.
        
                when "X-NODEREF":U then
                  phObjectHandle = ?.
        
                when "BUFFER":U then
                do:
                  if phObjectHandle:available then
                    phObjectHandle:buffer-release().
                end.
        
              end case.
              assign
                ttObjectList.lObjectInUse  = false
              .
            end.
          end.
        
    end method.
    
    method protected void loadRecordNode(
        input piRequestNo   as integer, 
        input phNode        as handle,  
        input piTransNo     as integer, 
        input piNodeLevel   as integer, 
        input  plDeleteTran  as logical 
     ) :
    /*------------------------------------------------------------------------------
      Purpose:     This procedure loads a record node into the temp-table that
                   it belongs to.
      Parameters:  
        
      Notes:       
    ------------------------------------------------------------------------------*/
      
      define variable hChildNode            as handle     no-undo.
      define variable iCount                as integer    no-undo.
      define variable lSuccess              as logical    no-undo.
      define variable cDBName               as character  no-undo.
      define variable cTableName            as character  no-undo.
      define variable cMessage              as character  no-undo.
      define variable iTableNo              as integer    no-undo.
      define variable cCurrNumSep         as character  no-undo.
      define variable cDatasetNumSep      as character  no-undo.
      define variable cCurrNumDec         as character  no-undo.
      define variable cDatasetNumDec      as character  no-undo.
    
      /* With a decimal, we need to swap the numeric format so that it
         is the same as it was in the incoming dataset */
      cCurrNumSep = session:numeric-separator.
      cDatasetNumSep = getAttribute(piRequestNo,"NumericSeparator":U).
      cCurrNumDec = session:numeric-decimal-point.
      cDatasetNumDec = getAttribute(piRequestNo,"NumericDecimal":U).
      
      define buffer bttTableList for ttTableList.
      
      if phNode:name = "contained_record":U then
      do:
        if can-do(phNode:attribute-names,"Table":U) then
        do:
          cDBName    = phNode:get-attribute("DB":U).
          cTableName = phNode:get-attribute("Table":U).
          find first bttTableList no-lock
            where bttTableList.iRequestNo = piRequestNo
              and bttTableList.cDBName    = cDBName
              and bttTableList.cTableName = cTableName no-error.
          if not available(bttTableList) then
          do:
            
             undo, throw new ApplicationError(subst("Failed to load record for &1 &2",cDBName,cTableName)).
          //  cMessage = {af/sup2/aferrortxt.i 'AF' '1999' '?' '?' cDBName cTableName}.
            
          //  return cMessage.
          end.
          iTableNo   = bttTableList.iTableNo.
          if cDatasetNumSep <> ? and
             cDatasetNumSep <> "":U and
             cDatasetNumDec <> ? and
             cDatasetNumDec <> "":U then
            session:set-numeric-format(cDatasetNumSep, cDatasetNumDec).
          
          setNodeValue
            (piRequestNo,
             piNodeLevel,
             iTableNo,
             "oRVObj":U,
             0,
             string("0.0"), //readRecordVersionAttr(piRequestNo, phNode)),
             yes).
          session:set-numeric-format(cCurrNumSep, cCurrNumDec).
        end.
     //   else if plDeleteTran then
        //  readRecordVersionAttr(piRequestNo, phNode).
    
      end.
      
      /* Create a Child Node ref */
      create x-noderef hChildNode.
    
      /* Iterate through the node's children */
      repeat iCount = 1 to phNode:num-children:
        
        
        /* Set the current Child Node */
        phNode:get-child(hChildNode,iCount).
    
        /* If the child node is not a node, skip it */
        if hChildNode:subtype <> "ELEMENT":U then
          next.
    
        /* If the child node is a contained record, recurse this call into this procedure again */
        if hChildNode:name = "contained_record":U then
          loadRecordNode (piRequestNo, hChildNode, piTransNo, piNodeLevel + 1, plDeleteTran).
    
        /* Otherwise assume that this is a field on this record */
        else
        do:
          assignNode(piRequestNo, piNodeLevel, iTableNo, hChildNode).
        end.
    
        if return-value <> "":U and
           return-value <> ?    then
        do:
          delete object hChildNode.
          hChildNode = ?.
          undo, throw new ApplicationError(subst("Load Record Node failed: &1",return-value)).   
          
        end.
      
      end. /* REPEAT iCount = 1  */
      
      /* Delete the child node */
      delete object hChildNode.
      hChildNode = ?.
      
      if piNodeLevel > 0 and
         iTableNo > 0 then
      do:
          writeTempTableData (piRequestNo, piNodeLevel, iTableNo, piTransNo).
      end.
    
    end method.
    
    method protected character assignNode  
          ( input piRequestNo as integer, 
            input piNodeLevel as integer, 
            input piTableNo   as integer, 
            input phParent    as handle) :
        /*------------------------------------------------------------------------------
          Purpose:  This procedure is responsible for pulling data from the lower level
                    of an field node into a database record.
            Notes:  
        ------------------------------------------------------------------------------*/
          
          define variable hNode        as handle     no-undo.
          define variable lSuccess     as logical    no-undo.
          define variable iCount       as integer    no-undo.
          define variable cValue       as character  no-undo.
          define variable iExtentIndex as integer    no-undo.
        
          /* Set the node to look at the next child */
          create x-noderef hNode.
        
          /* Iterate through the children */
          repeat iCount = 1 to phParent:num-children:
            /* Set the node to the child node */
            lSuccess = phParent:get-child(hNode,iCount).
            if not lSuccess then
              next.
        
            /* If this is not a text node, skip it */
            if hNode:subtype <> "TEXT":U then
              next.
        
            cValue = replace(hNode:node-value, chr(10), "":U).
            cValue = replace(cValue, chr(13), "":U).
            cValue = trim(cValue).
        
            /* Skip any blank values in the contained_record node that
               we may have put there for SCM */
            if cValue = "":U and
               phParent:name = "contained_record":U then
              next.
            else 
              cValue = hNode:node-value.
        
            if can-do(phParent:attribute-names,"ExtentIndex":U) then
              iExtentIndex = integer(phParent:get-attribute("ExtentIndex":U)).
            else
              iExtentIndex = 0.
        
            setNodeValue
              (piRequestNo,
               piNodeLevel,
               piTableNo,
               phParent:name,
               iExtentIndex,
               cValue,
               yes).
        
          end.
        
          delete object hNode.
          hNode = ?.
        
        
          return "":U.   /* Function return value. */

    end method.
    
    
    method protected logical setNodeValue  
      ( input piRequestNo   as integer,
        input piNodeLevel   as integer,
        input piTableNo     as integer,
        input pcNodeName    as character,
        input piExtentIndex as integer,
        input pcValue       as character,
        input plDelete      as logical) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
      define buffer bttNode for ttNode.
      /* Write a record into the table for this field */
      do transaction:
        find first bttNode 
          where bttNode.iRequestNo   = piRequestNo
            and bttNode.iLevelNo     = piNodeLevel
            and bttNode.iTableNo     = piTableNo
            and bttNode.cNode        = pcNodeName
            and bttNode.iExtentIndex = piExtentIndex
         no-error.
        if not available(bttNode) then
        do:
          create bttNode.
          assign
            bttNode.iRequestNo = piRequestNo
            bttNode.iLevelNo   = piNodeLevel
            bttNode.iTableNo   = piTableNo
            bttNode.cNode      = pcNodeName
            bttNode.iExtentIndex = piExtentIndex
          .
        end.
        error-status:error = no.
        assign
          bttNode.cValue     = pcValue
          bttNode.lDelete    = plDelete
        .
      end.

     return true.   /* Function return value. */

    end method.
    
    method protected logical registerObject  
      ( input piRequestNo as integer,
        input pcHandle    as character,
        input phHandle    as handle ) :
        /*------------------------------------------------------------------------------
          Purpose:  Sets the value of properties in the ttReqHandle table.
            Notes:  
        ------------------------------------------------------------------------------*/
          define variable cHandles as character  
            initial "QUERY,BUFFER,TEMP-TABLE,X-NODEREF,X-DOCUMENT":U
            no-undo.
        
          define buffer bttReqHandle for ttReqHandle.
        
          
          /* Find the ttReqHandle record, creating it if necessary, and set the 
             value of the property to the value in the input parameter */
          do for bttReqHandle:
            find bttReqHandle
              where bttReqHandle.iRequestNo = piRequestNo
                and bttReqHandle.cHandleName = pcHandle
              no-error.
            if not available(bttReqHandle) then
            do:
              create bttReqHandle.
              assign
                bttReqHandle.iRequestNo = piRequestNo
                bttReqHandle.cHandleName = pcHandle
                .
            end.
            assign
              bttReqHandle.cHandleType = phHandle:type
              bttReqHandle.iDelOrder = lookup(phHandle:type, cHandles)
              bttReqHandle.hHandle  = phHandle
              .
          end.
          return true.
    end method.
    
    method protected handle getObjectHandle  
      ( input piRequestNo as integer,
        input pcHandleName as character ) :
    /*------------------------------------------------------------------------------
      Purpose:  Returns the value associated with a request handle.
        Notes:  
    ------------------------------------------------------------------------------*/
      define buffer bttReqHandle for ttReqHandle.
      define variable hRetVal    as handle  no-undo.
    
      /* Find the ttReqHandle record and set the return value 
         to the value of the property */
      do for bttReqHandle:
        find bttReqHandle
          where bttReqHandle.iRequestNo = piRequestNo
            and bttReqHandle.cHandleName = pcHandleName
          no-error.
        if not available(bttReqHandle) or 
           not VALID-HANDLE(bttReqHandle.hHandle) then
          hRetVal = ?.
        else
        do:
          hRetVal = bttReqHandle.hHandle.
          /* Sanity check. If the handle is not the same type of handle as
             we originally created, there's a problem. This will trigger
             a cleanup of the whole request */ 
          if hRetVal:type <> bttReqHandle.cHandleType then
          do:
            delete object hRetVal.
            hRetVal = ?.
            delete bttReqHandle.
          end.
        end.
      end.
      return hRetVal.   /* Function return value. */

    end method.
    
        
    method protected character getAttribute  
          ( input piRequestNo as integer,
            input pcAttribute as character ) :
        /*------------------------------------------------------------------------------
          Purpose:  Returns the value associated with a dataset attribute.
            Notes:  
        ------------------------------------------------------------------------------*/
          define buffer bttDSAttribute for ttDSAttribute.
          define variable cRetVal as character  no-undo.
        
          /* Find the ttDSAttribute record and set the return value 
             to the value of the property */
          do for bttDSAttribute:
            find bttDSAttribute
              where bttDSAttribute.iRequestNo = piRequestNo
                and bttDSAttribute.cAttribute = pcAttribute
              no-error.
            if not available(bttDSAttribute) then
              cRetVal = ?.
            else
              cRetVal = bttDSAttribute.cValue.
          end.
          return cRetVal.   /* Function return value. */

    end method.
    
    method protected logical setAttribute  
       (input piRequestNo   as integer,
        input pcAttribute   as character,
        input pcValue       as character ) :
        /*------------------------------------------------------------------------------
      Purpose:  Sets the value of attributes in the ttDSAttribute table.
        Notes:  
    ------------------------------------------------------------------------------*/
    
        define buffer bttDSAttribute for ttDSAttribute.
    
    
      /* Find the ttDSAttribute record, creating it if necessary, and set the 
         value of the property to the value in the input parameter */
        do for bttDSAttribute:
            find bttDSAttribute
            where bttDSAttribute.iRequestNo = piRequestNo
            and bttDSAttribute.cAttribute = pcAttribute
            no-error.
            if not available(bttDSAttribute) then
            do:
                if pcValue <> ? then
                do:
                    create bttDSAttribute.
                    assign
                        bttDSAttribute.iRequestNo = piRequestNo
                        bttDSAttribute.cAttribute = pcAttribute
                        .
                end.
            end.
            error-status:error = no.
    
            if pcValue <> ? then
            assign
                bttDSAttribute.cValue  = pcValue
            .
            else
            do:
                if available(bttDSAttribute) then
                    delete bttDSAttribute.
            end.
        end.
    end method.
 
    
    method logical deleteAttributes 
        (input piRequestNo as integer ) :
        /*------------------------------------------------------------------------------
      Purpose: Loops through the table list and deletes all the attributes in the
               table for a particular request. 
        Notes: If piRequestNo is unknown, all the attributes in the table are 
               deleted, otherwise only the requested request's attributes are 
               deleted. 
    ------------------------------------------------------------------------------*/
        define buffer bttDSAttribute for ttDSAttribute.
        define query qAttList for bttDSAttribute.
    
        /* Open the appropriate version of the query */
        if piRequestNo = ? then
            open query qAttList
               for each bttDSAttribute.
        else
            open query qAttList
               for each bttDSAttribute
                   where bttDSAttribute.iRequestNo = piRequestNo.
    
        get first qAttList.
    
        /* Loop through the records in the query */
        repeat while available(bttDSAttribute):
          
           /* Delete the table record. */
            delete bttDSAttribute.
     
            /* Get the next record */
            get next qAttList.
        end.
    
        /* Close the query */
        close query qAttList.
        
        return true.   /* Function return value. */

    end method.
    
    method logical deleteReqHandles 
         (input piRequestNo as integer ) :
        /*------------------------------------------------------------------------------
          Purpose: Loops through the table list and deletes all the attributes in the
                   table for a particular request. 
            Notes: If piRequestNo is unknown, all the attributes in the table are 
                   deleted, otherwise only the requested request's attributes are 
                   deleted. 
        ------------------------------------------------------------------------------*/
          define buffer bttReqHandle for ttReqHandle.
          define query qReqHandle for bttReqHandle.
        
          /* Open the appropriate version of the query */
          if piRequestNo = ? then
            open query qReqHandle
              for each bttReqHandle 
                by bttReqHandle.iRequestNo
                by bttReqHandle.iDelOrder.
          else
            open query qReqHandle
              for each bttReqHandle
                 where bttReqHandle.iRequestNo = piRequestNo
                 by bttReqHandle.iRequestNo
                 by bttReqHandle.iDelOrder
                 by bttReqHandle.cHandleName.
        
          get first qReqHandle.
        
          /* Loop through the records in the query */
          repeat while available(bttReqHandle):
              
            if valid-handle(bttReqHandle.hHandle) then
              delete object bttReqHandle.hHandle.
        
            /* Delete the table record. */
            delete bttReqHandle.
         
            /* Get the next record */
            get next qReqHandle.
          end.
        
          /* Close the query */
          close query qReqHandle.
            
          return true.   /* Function return value. */

    end method.
    
    
    method public logical deleteTempTables
        (input piRequestNo as integer ) :
        /*------------------------------------------------------------------------------
          Purpose: Loops through the table list and deletes all the temp-tables in the
                   table list. 
            Notes: If piRequestNo is unknown, all the temp-tables in the TT List are 
                   deleted, otherwise only the requested temp-table is deleted. 
        ------------------------------------------------------------------------------*/
          define buffer bttTableList for ttTableList.
          define query qTTList for bttTableList.
        
          /* Open the appropriate version of the query */
          if piRequestNo = ? then
            open query qTTList
              for each bttTableList.
          else
            open query qTTList
              for each bttTableList
                 where bttTableList.iRequestNo = piRequestNo.
        
          get first qTTList.
        
          /* Loop through the records in the query */
          repeat while available(bttTableList):
              
            /* If the handle is valid, trash the object */
           // if valid-handle(bttTableList.hTable) then
             // run releasePoolObject /* IN ghObjectPool */
               // (bttTableList.hTable).
        
            /* Delete the table record. */
            delete bttTableList.
         
            /* Get the next record */
            get next qTTList.
          end.
        
          /* Close the query */
          close query qTTList.
            
          return true.   /* Function return value. */

    end method.
    
    method protected void loadTransactionNodes(
      input  piRequestNo   as integer, 
      input  phNode        as handle, 
      input  plImportData  as logical
    ) :
    /*------------------------------------------------------------------------------
      Purpose:     Loads the transaction nodes and processes them.
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
      define variable iTransNo    as integer    no-undo.
      define variable hChildNode  as handle   no-undo.
      define variable iCount      as integer    no-undo.
      define variable lSuccess    as logical    no-undo.
      define variable lDelete     as logical    no-undo.
      define variable lOverwrite  as logical    no-undo.
      define variable cVal        as character  no-undo.
      define variable lError      as logical    no-undo.
      define variable cTranType   as character  no-undo.
      define variable lDeleteTran as logical    no-undo.
    
      define buffer bttTransaction for ttTransaction.
    
      lError = no.
      
      /* Create a Child Node ref */
      create x-noderef hChildNode.
    
      /* Iterate through the root node's children */
      repeat iCount = 1 to phNode:num-children:
       // run clearRetVal.
        error-status:error = no.
        
        /* Set the current Child Node */
        phNode:get-child(hChildNode,iCount).
        
        /* If the child node is not the dataset records child node, skip it */
        if hChildNode:subtype = "TEXT":U or
           hChildNode:name <> "dataset_transaction":U then
          next.
    
        iTransNo    = integer(hChildNode:get-attribute("TransactionNo":U)).
    
        lDeleteTran = no.
    
        if can-do(hChildNode:attribute-names,"TransactionType":U) then
        do:
          cTranType    = hChildNode:get-attribute("TransactionType":U).
          if cTranType = "DELETION":U then
            lDeleteTran = yes.
        end.
        
    
        /* Now we need to pass this node on to the record processor that
           will load the contents of the node into temp-tables */
    //    publish "DSAPI_StatusUpdate":U 
  //          ("Loading Dataset Transaction: " + STRING(iTransNo)).
    
        empty temp-table ttNode.
    
        error-status:error = no.
      //  run clearRetVal.
        
        loadRecordNode (piRequestNo, hChildNode, iTransNo, 0, lDeleteTran).
    
        if error-status:error or 
           (return-value <> "":U and
            return-value <> ?) then
        do:
        //  run writeNodeToErrorLog (piRequestNo, hChildNode, "999|" + return-value).
          lError = yes.
          error-status:error = no.
        end.
    
        empty temp-table ttNode.
        
        if plImportData and
           (return-value = "":U or
            return-value = ?) then 
        do:
    
          cVal = getAttribute(piRequestNo,"OverwriteNodes":U).
          if cVal = ? then
            lOverwrite = yes.
          else
            lOverwrite = cVal = "YES":U.
    
          cVal = getAttribute(piRequestNo,"DeleteExisting":U).
          if cVal = ? then
            lDelete = yes.
          else
            lDelete = cVal = "YES":U.
          
        //  publish "DSAPI_StatusUpdate":U 
          //  ("Writing Dataset to Database: " + STRING(iTransNo)).
    
          error-status:error = no.
        //  run clearRetVal.
          /* If this is a deletion then apply the deletion */
       //   if lDeleteTran then 
         //   run removeDeletions (piRequestNo).
        //  else
            /* This code has to deal with writing the stuff to the database */
      //      run writeDataToDB (piRequestNo, hChildNode, yes, lDelete, lOverwrite).
    
          if return-value = "EDO":U then
          do:
            assign
              lError = yes
            .
            error-status:error = no.
          end.
        end.
        
      end. /* REPEAT iCount = 1  */
      
      delete object hChildNode.
      hChildNode = ?.
    
      /* This is just to notify the caller that an EDO file needs to be written. */
      if (return-value <> "":U and
          return-value <> ?) then
            undo, throw new ApplicationError(return-value).      
       else if lError then 
          undo, throw new ApplicationError("Load Transaction failed").      
    end method.
    
    
    
end class.
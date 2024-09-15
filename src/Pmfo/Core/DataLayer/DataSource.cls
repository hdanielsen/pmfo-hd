 
/*------------------------------------------------------------------------
   File        : DataSource
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hdaniels
   Created     : Wed Feb 13 10:20:47 EST 2019
   Notes       : 
 ----------------------------------------------------------------------*/
 
block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.IUpdateDataRequest from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.List from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.Core.WidgetHandle from propath.
using Pmfo.Core.BusinessLogic.CompareQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryBuilder from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Common.IQuestion from propath.
using Pmfo.Core.Common.IWarning from propath.
using Pmfo.Core.Common.LifecycleScope from propath.
using Pmfo.Core.Common.Question from propath.
using Pmfo.Core.Common.Warning from propath.
using Pmfo.Core.DataLayer.ConstantField from propath.
using Pmfo.Core.DataLayer.DataSource from propath.
using Pmfo.Core.DataLayer.DataSourceInnerJoinMap from propath.
using Pmfo.Core.DataLayer.DbQuery from propath.
using Pmfo.Core.DataLayer.FieldProxy from propath.
using Pmfo.Core.DataLayer.IConstantField from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.
using Pmfo.Core.DataLayer.IFieldProxy from propath.
using Pmfo.Core.DataLayer.IFieldSaveManager from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.LogicalFieldProxy from propath.
using Pmfo.Core.DataLayer.Update.ProxyManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.ConflictError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Error.TransactionError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Lang.AppError from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Core.DataLayer.Update.SourceFieldManager from propath.
using Pmfo.Core.DataLayer.ISourceField from propath.
using Pmfo.Core.DataLayer.Util.SaveUtil from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Core.Common.FieldValue from propath.
using Pmfo.Core.Common.Key from propath.
 

class Pmfo.Core.DataLayer.DataSource abstract inherits DbQuery implements IUpdateTarget:
    /** See GetRecordCheckSum
    define private temp-table ttRawField no-undo 
        field rawfield as raw.
    **/
    
    define protected property FieldSaveManager as IFieldSaveManager no-undo get. set.
     
    /* requested number */
    define public abstract property EntityName   as char no-undo get.  
    define public    property NumRecords         as int64     no-undo get. protected set.
    // set to true to avoid lobs in save row changes (i.e. when images are stored to disk)
    define protected property NoLobs             as logical   no-undo get. protected set.
    
    // set for feidls that is not supposed to be optimitic locked checked or saved automatically
    define protected property SaveExceptFields   as character init ? no-undo get. protected set.
    
    define protected property ConfirmedQuestions as character no-undo get. set.
    
    define protected property Questions as IMap no-undo
        get().
            if not valid-object(this-object:Questions) then
                this-object:Questions = new Map().
            return Questions.
        end.
        protected set.
    
    define public    property Count                   as logical   no-undo get. protected set.
    define public    property TotalCount              as int64     no-undo init ? get. protected set.
    define protected property HasWarning              as logical   no-undo get. private set.
    // to be set in GetBufferHandles when the request is a many-to-one join where 
    // batch-size does not work  
    define protected property UseBeforeFillWithPaging as logical   no-undo get. set.
    // set in prepare to use by BeforeFillWithPaging
    define protected property RestartRow              as int64   no-undo get. set.
    // set in prepare to use by BeforeFillWithPaging
    define protected property RestartIds              as rowid  extent no-undo get. set.
    define protected property DataSort                as character no-undo get. set.
    
    define public property FillErrors     as ICollection no-undo 
        get():
            if not valid-object(FillErrors) then
                FillErrors = new Collection().  
            return FillErrors.         
        end.
        private set.
     
    // Don't create andprepare the query, but call BufferHandles with requests 
    // (See also inherited NoConnnectNeeded, which is to be used when buffers are to be defined later 
    define protected property NoQueryCreate as logical   no-undo get. set. 
    
    // Comma separated list of names of fields that are mapped to different dasa type in db 
    // - typically managed by a FieldProxy and AddFieldProxy updates this list  
    //   but as of current also set directly to manage keys/foreignkeys in generic tables and specifically to tell 
    //   the DataSource to use BeforeFillForProxyJoin  to join to keys
    // TODO unify -use proxy fields always ? (there are certainly complications with foreignkesy
    define protected property ProxyFieldNames      as character no-undo get. set. 
    define protected property ProxyManager         as ProxyManager no-undo get. set. 
    define protected property SourceFieldManager   as SourceFieldManager no-undo get. set. 
    
    define public property QueryStringWithoutParent as character  no-undo 
        get():
            if QueryStringWithoutParent = "" and valid-handle(QueryHandle) then 
            do: 
                return QueryHandle:prepare-string. 
            end.    
            return QueryStringWithoutParent.   
        end.      
        set. 
    define public    property Warnings       as IList     no-undo 
        get. 
        protected set(pWarns as IList):
            Warnings = pWarns.
            HasWarning = true.
        end. 
    
    define protected property SeqFieldHandle as handle    no-undo get. set.
    define protected property UpdateSource   as integer   no-undo 
        get():
            define variable i as integer no-undo.
            if UpdateSource = 0 then
            do:
                AssertBufferHandles("Get UpdateSource").
                do i = 1 to extent(BufferHandles):
                    if BufferHandle = BufferHandles[i] then
                    do:
                        UpdateSource = i.
                        leave.
                    end.    
                end.    
            end.
            return UpdateSource.
        end.     
        set.
   
    define public    property DataBuffer     as handle    no-undo 
        get. 
        set(pBuffer as handle):
            // static SeqFieldName  
            SeqFieldHandle = pBuffer:buffer-field(BufferUtil:SeqFieldName) no-error.
            DataBuffer = pBuffer.
        end. 
    
    define protected property ExternalPaging  as logical no-undo get. set.
    define protected property RecordCounter   as int64   no-undo get. set.    
    define protected property FieldMap        as character init ? no-undo
        get():
            define variable cMap as character no-undo.
            define variable i    as integer no-undo.
            AssertBufferHandles("Get FieldMap").
            cMap = GetFieldMap().
            // Map some commonly used names if this is a join
            // Progress will map to the last matching table if not specified. 
            // This is typically wrong and causes very serious bugs when the 
            // mapping is not specified in GetFieldMap, which is easy to forget/overlook  
            // TODO - this can be removed if all datasources getFieldMap calls super - lots of code though. 
            if extent(BufferHandles) > 1 then 
            do:
                cMap = CheckAndAddFieldToMap("note-id",cMap).
                cMap = CheckAndAddFieldToMap("descr",cMap).
                cMap = CheckAndAddFieldToMap("name",cMap).
                cMap = CheckAndAddFieldToMap("obsolete",cMap).
            end.
            
            return cMap.
        end.    
    
    define protected property DataSourceHandle as handle no-undo
        get. 
        set.               
    
    // This just be set to "" in prepare
    // It is typically called after fill when npaging when DataSourcehandle:next-rowid has data
    // But ut can also be assigned in cases where the paging has to be managed manually in before-fill  
    define public property NextRowKey as character no-undo  
        get():
            define variable iSource         as integer   no-undo.
            define variable hSource         as handle    no-undo.
            define variable rRowid          as rowid     no-undo.
            if NextRowKey = "" or NextRowKey = ? then 
            do:
                do iSource = 1 to extent(BufferHandles):
                    if BufferHandle = BufferHandles[iSource] then 
                        leave.
                end.  
                if not valid-handle(DataSourcehandle) then 
                    return ?. // Added after removing create datasource from the setter since it caused memory leak on save (report execute)
                             //  TODO can this ever happen    
                hSource = DataSourcehandle:get-source-buffer (iSource).
                rRowid = DataSourcehandle:next-rowid(iSource).
               // assumed to be because we're not paging 
                if rRowid = ? then 
                    return ?.
                NextRowKey =  GetNextRowKey(hSource, rRowid).     
            end.
            return NextRowKey.
        end.     
        private set.      
                  
    define protected property ParentRelation as handle no-undo 
        get():
            if not valid-handle(ParentRelation) then do:
                if valid-handle(DataBuffer) then 
                do:
                    ParentRelation = DataBuffer:parent-relation. 
                end.
            end.
            return ParentRelation.
        end. 
        
        private set.
        
    constructor public DataSource ( input phDbBuffer as handle ):
        super (input phDbBuffer).
    end constructor.
    
    // careful - for datasources that get data from non-db sources  
    constructor protected DataSource ( ):
        super ().
    end constructor.
    
    method protected handle extent GetBufferHandles ( input pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, input pParentRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest ):
        return GetBufferHandles().
    end method.
    
    // allow subclasses to customize alreayd exists error - default to replace db table with GetRowname the 
    method protected character GetPublicAlreadyExistsMessage(pcMessage as character,phBeforebuffer as handle, piSource as integer):
        return replace(pcMessage,BufferHandles[piSource]:name,BufferUtil:GetRowName(phBeforeBuffer)).
    end method.
    
    
    method protected character GetDataSourceKeys ( pDatasourcebuffer as handle ):
        return pDatasourcebuffer:keys.
    end method.
    
    method protected handle extent GetSaveBufferHandles ():
        return GetBufferHandles().
    end method.
    
    method protected IHandleDataSourceMap GetDataSourceMap(phHandle as handle):
        return cast(cast(Ccs.Common.Application:ServiceManager,IServiceManager):getService(get-class(IHandleDataSourceMap), new LifecycleScope(phHandle)),IHandleDataSourceMap). 
    end.    
    
    /* DataAccess subscribes this to the Beforeupdate event fired before the update starts
       This fires outside the transaction
       Subclasses overrides this if they need to do something that cannot 
       be handled in the row level events 
       The override must do a for each to get the data   
    */ 
    method public void BeforeUpdate (input dataset-handle phDataset):
    end method.
    
    /* DataAccess subscribes this to the Afterupdate event fired when the update ends
       but before refresh of data
       This fires outside the transaction
       Subclasses overrides this if they need to do something that cannot 
       be handled in the row level events 
       The override must do a for each to get the data   
    */ 
    method public void AfterUpdate (input dataset-handle phDataset).
    end method.
    
    /* Subscribes this to the event fired at the beginning of the transaction
       This is only subscribed in the top level UpdateTarget 
       It is not fired for CommitScopeEnum:Row 
       Subclasses overrides this if they need to do something that cannot 
       be handled in the row level events 
           
    */ 
    
    method public void BeginTransaction (input dataset-handle phDataset):
    end method.
    
    method public void EndTransaction (input dataset-handle phDataset):
    end method.
    
    method protected void BufferError(hBuffer as handle,pcMsg as character):
        ErrorStringUtil:AddError(hBuffer, pcMsg).
    end method.    
    
    method protected void BufferError(hBuffer as handle,pcMsg as character, pcField as character):
        ErrorStringUtil:AddError(hBuffer, pcMsg, pcField).
    end method.    
    
    method protected void ThrowBufferError(hBuffer as handle,pcMsg as character):
        ErrorStringUtil:AddError(hBuffer, pcMsg).
        undo, throw new TransactionError().
    end method. 
    
    method protected void ThrowBufferError(hBuffer as handle,pcMsg as character, pcField as character):
        ErrorStringUtil:AddError(hBuffer, pcMsg, pcField).
        undo, throw new TransactionError().
    end method. 
    
    method protected void BufferError(pcMsg as character):
        BufferError(GetAndAssertErrorBuffer(),pcMsg). 
    end method.    
    
    method protected void BufferError(pcMsg as character, pcField as character):
        BufferError(GetAndAssertErrorBuffer(),pcMsg,pcField).
    end method.    
    
    method protected void ThrowBufferError(pcMsg as character):
        ThrowBufferError(GetAndAssertErrorBuffer(),pcMsg). 
    end method. 
    
    method protected void ThrowBufferError(pcMsg as character, pcField as character):
        ThrowBufferError(GetAndAssertErrorBuffer(),pcMsg,pcField).
    end method. 
    
    method protected void ThrowFillError(pcMsg as character):
        ThrowFillError(new ApplicationError(pcMsg)).
    end method. 
    
    method protected void ThrowFillError(pError as Error):
        // add to collection so that DataAccess:Fill can find it
        FillErrors:Add(pError).
        // throw so that Fill returns false (also written to log)
        undo, throw pError.  
    end method. 
    
    method protected void AddWarning(pcMsg as character):
        AddWarning(new Warning(pcmsg)). 
    end method.
    
    method protected void AddWarning(pcMsg as character, pcField as character):
        AddWarning(new Warning(pcmsg,pcField)). 
    end method.
    
    method protected void AddWarning(pWarning as IWarning):
        if not valid-object(Warnings) then
        do:
            Warnings = new List().
        end.    
        Warnings:Add(pWarning).
    end method.
    
    method protected void AddWarnings(pWarnings as ICollection):
        define variable oIter as IIterator no-undo.
        define variable oObject as Object no-undo.
        if not valid-object(Warnings) then
        do:
            Warnings = new List().
        end.    
        oIter = pWarnings:Iterator().
        do while oIter:HasNext():
            oObject = oIter:Next().
            if type-of(oObject,IWarning) = false then
                undo, throw new IllegalArgumentError(subst("Collection with &1 objects",oObject:GetClass():TypeName)). 
        end.     
        Warnings:AddAll(pWarnings).
    end method.
    
    method protected handle GetAndAssertErrorBuffer():
        if DataBuffer:before-buffer:avail and DataBuffer:before-buffer:row-state = row-deleted then
            return DataBuffer:before-buffer.
        else if DataBuffer:Avail = false then
        do:
            undo, throw new IllegalOperationError(subst("Error in error handling: Buffer '&1' is not available",DataBuffer:name)).
        end.         
        return DataBuffer.    
    end method.
    
    // ovverride to customize field mappoing 
    method protected character GetFieldMap(): 
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        define variable cExceptList as character no-undo.
        define variable cDlm        as character no-undo.
        define variable cKeys       as character no-undo.
        define variable i           as integer no-undo.
        define variable iThisbuffer as integer no-undo.
        define variable hFld as handle no-undo.
        define variable IKey as integer no-undo.  
        define variable cKeymap as character no-undo.
        define variable cmap as character no-undo.
        define variable j as integer no-undo.
        
        if extent(BufferHandles) = 1 then 
            return "".
                     
       // if valid-object(ProxyManager) then
       //  as of current ProxyFieldNames is managed in AddProxyField and also set directly
       //  to manage keys and specifically join to keys in sources mapped to generic tables
       // TODO use FieldProxy always (FieldMap)    
        cExceptList = ProxyFieldNames.
             
        // if main buffer is not first buffer then check if key can be mapped to an earlier table
        // this may seem a bit loose and odd, but it is actually what will happen in a case where 
        // no fieldmap is provided in attach 
        // -  the GetFieldMAp used to return "" so this logic was added to preserve
        //    similar behavior after adding new logic here that ensures the fields are
        //    mapped correctly for the main table 
        // -  without  this change mapping the key to the main table when not first in query will cause   
        // error  Unable to auto-generate a FILL query predicate for <table> if field <keyfield in <ttname> is not mapped. (11883)"
         
        if Bufferhandle <> BufferHandles[1] then 
        do:
            cKeys = Bufferhandle:keys.
            
            iThisbuffer = lookup(BufferName,BufferNames).
            do i = 1 to iThisBuffer - 1 : 
                do ikey = 1 to num-entries(cKeys): 
                    hFld = BufferHandles[i]:buffer-field(entry(i,cKeys)) no-error.
                    if valid-handle(hFld) then 
                    do: 
                        ckeymap = cKeymap 
                            + (if cKeyMap = "" then "" else ",")
                            + subst("&1,&2.&1",entry(i,ckeys),BufferHandles[i]:name).  
                        cExceptList = cExceptList 
                            + (if cExceptList = "" then "" else ",")
                            + entry(i,ckeys).  
                                                            
                    end. 
                end.
                do j = 1 to num-entries(cExceptlist):
                    iKey = lookup(entry(j,cexceptList),cKeys).
                    if IKey > 0 then 
                    do:
                        StringUtil:RemoveEntryFromList(iKey,cKeys).
                    end.        
                end.        
            end.
        end. 
        cMap = BufferUtil:GetFieldMap(DataBuffer,BufferHandle,cExceptList).
        if cKeymap > "" then
            cMap = trim(subst("&1,&2",cMap,cKeymap),","). 
        return cMap.    
    end method.
     
    /* override to attach after row event and/or bind the buffer to static def */ 
    method protected void PrepareBuffer ( input pBuffer as handle):
        if valid-handle(SeqFieldHandle) then
            pBuffer:set-callback("After-Row-fill","AfterRow").
    end method. 
    
    /* override to attach after row event and/or bind the buffer to static def */ 
    method protected void PrepareSaveBuffer ( input pBuffer as handle):
    end method. 
    
    method override protected void CreateQuery():
    
        super:CreateQuery().
        if valid-object(DataSourcehandle) then
            delete object DataSourcehandle. 
            
        create data-source DataSourcehandle.
       
        DataSourceHandle:query = QueryHandle.
    end method.   
    
    // override by subclass to add fixed query expressions dynamically (intended for abstract application aubclass ) 
    // for example adding deleted = false on databases that uses logical field to define records as deleted  
    method protected character PrepareBaseQuery(pcQuery as character,pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest):
    end method.    
    
    method protected void PrepareCustomRequest (pCustomObject as Object):
        define variable oAction as IAction no-undo.
        define variable oMethod as Method no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable i as integer no-undo.
        define variable hFld as handle no-undo.
        if type-of(pCustomObject,IAction) then
        do:
            oAction = cast(pCustomObject,IAction).
            oMethod = GetActionMethod(oAction, DataBuffer:dataset, oParameterList).
            if valid-object(oMethod) then
            do:
                 
                if oAction:CompareFields > "" then
                do:
                    do i = 1 to num-entries(oAction:CompareFields): 
                        hFld = DataBuffer:buffer-field(entry(i,oAction:CompareFields)) no-error.
                        if not valid-handle(hFld) then 
                        do:
                           
                            undo, throw new IllegalStateError(subst("Action '&1' CompareFields entry '&2' does not exist in '&3'",
                                oAction:name,
                                entry(i,oAction:CompareFields),
                                BufferUtil:GetRowName(Databuffer))).
                        end.        
                    
                    end.
                end.    
                PrepareAction(cast(pCustomObject,IAction)).
            end.
        end.
        // as of current INamedQuery is mutually exclusive from action
        // this may need to change  
        else if type-of(pCustomObject,INamedQuery) then
        do:   
            PrepareNamedQueryForUpdate(cast(pCustomObject,INamedQuery)).   
        end.    
    end method.
   
    // no default behavior  
    method protected logical PrepareAction( pAction as IAction):
    end method.
    
    // no default behavior  
    method protected void PrepareNamedQueryForUpdate( pNamedQuery as INamedQuery):
    end method.
    
    // private -  quick and dirty may need improvements
    method private character TrimSaveFieldMap(pcFieldMap as character,pcExceptlist as character):
        define variable i      as integer no-undo.
        define variable cField as character no-undo.
        define variable cDbName as character no-undo.
        define variable iLookup as integer no-undo.
        
        if pcFieldMap > "" and pcExceptList > "" then
        do i = 1 to num-entries(pcExceptList) :
            cfield = entry(i,pcExceptList).
            if cField > "" then
            do:
                iLookup = lookup(cField,pcfieldMap).
                if iLookup > 0 then 
                do:
                   // remvoe both entries
                    pcfieldMap = StringUtil:RemoveEntryFromList(iLookup, pcFieldMap) .
                   // should be the same ewntry since we rmeoved one before
                    pcfieldMap = StringUtil:RemoveEntryFromList(iLookup, pcFieldMap) .
                   
                end.
            end.
        end.     
        return pcFieldMap.
    end method.    
    
    // create the data-source and attach - override in subclass if cannot attach
    method protected void CreateSaveSource(phBuffer as handle, pcFieldMap as character, pcExceptList as character ):
        define variable i             as integer no-undo.
        define variable cHiddenFields as character no-undo.
        define variable cExcept       as character no-undo.
        define variable iJoinField    as integer no-undo.
        define variable lKeepattached as logical no-undo.
        if extent(BufferHandles) = ? or not valid-handle(BufferHandles[1])  then
            undo, throw new IllegalOperationError("CreateSaveSource in DataSource with no database buffers").
        if valid-object(DataSourcehandle) then
            delete object DataSourcehandle. 
        create data-source DataSourcehandle.
        
        do i = 1 to extent(BufferHandles):
            DataSourcehandle:add-source-buffer(BufferHandles[i],GetDataSourceKeys(BufferHandles[i])).
        end.    
        
        if pcExceptList = "" then 
            pcExceptList = ?. 
        
        pcFieldMap = TrimSaveFieldMap(pcFieldMap, pcExceptlist).
        
        cHiddenFields = BufferUtil:GetHiddenFields(phBuffer).
        do i = 1 to num-entries(pcExceptList): 
            cExcept = entry(i,pcExceptlist).
            if lookup(cExcept,cHiddenFields) > 0 then 
                cHiddenFields = StringUtil:RemoveEntryFromList(cExcept,cHiddenFields).
        end. 
        if lookup(BufferUtil:SeqFieldName,cHiddenFields) > 0 then 
            cHiddenFields = StringUtil:RemoveEntryFromList(BufferUtil:SeqFieldName,cHiddenFields).
         
        phBuffer:attach-data-source(DataSourceHandle,pcFieldMap,pcExceptList).
        if cHiddenfields > "" then  
        do: 
           
             // DEFECT OCTA-39132 
             // - Case #:  01028213 - work around -
            // was decided to not be a bug, but it is quite problematic when having hidden fields
            // need to set both to supress the reset of before values 
            // for unchanged hidden fields different than db  
            // used in SaveRow
            // This requires logic in saverow to apply the current value to the afte table
            // if there are no changes - otherwise the value, which will be set from default
            // and not from client for hidden fields will be saved and override the data  
            DataSourceHandle:merge-by-field = false.
            DataSourceHandle:prefer-dataset = true.
           
        end.
                
    end method.
    
    // allow subclass to override 
    method protected character PrepareAndGetSaveExceptList(phBuffer as handle,pUpdateRequest as IUpdateDataRequest):
        return ProxyFieldNames.
    end method.    
    
    method protected void DefineFieldProxies():
        
    end method.   
    /**
     The concrete classes will typically override PrepareSave(buffer) to Bind the table as follows 
       htable  = phbuffer:table-handle
       Bind(input table-handle hTable).
  **/       
    method final public logical PrepareSave( phBuffer as handle,pUpdateRequest as IUpdateDataRequest ):
        
         
        DataBuffer = phBuffer.
        // should possibly be in initialize? but trying to avoid depending on initialize to allow reuse ()    
        DefineFieldProxies().
        
        extent(BufferHandles) = ?.
        BufferHandles = GetSaveBufferHandles().
       
        // call PrepareSavebuffer before CreateSaveSource since overrides calls bind where we may create fieldProxy
        // that set ProxyFieldNames used in CreateSaveSource - TODO - move all proxies to  DefineFieldProxies().
  
        PrepareSaveBuffer(phBuffer).   
        
            
        if valid-object (ProxyManager) then 
            ProxyManager:Prepare(Databuffer, BufferHandle).
        
        if type-of(pUpdateRequest,Pmfo.Core.BusinessLogic.IUpdateDataRequest) then 
             PostPrepareSave(input phBuffer, cast(pUpdateRequest,Pmfo.Core.BusinessLogic.IUpdateDataRequest)).
        
        CreateSaveSource(phBuffer,FieldMap,PrepareAndGetSaveExceptList(phBuffer,pUpdateRequest)).
        
        if valid-object(pUpdateRequest) then
        do:
            if type-of(pUpdateRequest,Pmfo.Core.BusinessLogic.IUpdateDataRequest) then
            do: 
                ConfirmedQuestions = cast(pUpdateRequest,Pmfo.Core.BusinessLogic.IUpdateDataRequest):GetConfirmedQuestions(EntityName).
            end. 
            if valid-object(pUpdateRequest:CustomRequest) then
                PrepareCustomRequest(pUpdateRequest:CustomRequest).
        end.    
    end method. 
     
     // override in subcclass for example to call UpdateKeys to chang keys before child sources are started
    method protected void PostPrepareSave(phBuffer as handle,pUpdateRequest as Pmfo.Core.BusinessLogic.IUpdateDataRequest ):
    end.     
     
    method protected IFieldProxy SetFieldProxy(phfld as handle,phDbBuffer as handle, pcDbfieldName as character):
        return AddFieldProxy(new FieldProxy(phFld,phDbBuffer,pcDbfieldName)).
    end method.    
    
    method protected IFieldProxy SetLogicalFieldProxy(phfld as handle,phDbBuffer as handle, pcDbfieldName as character, pcformat as character):
        return AddFieldProxy(new LogicalFieldProxy(phFld,phDbBuffer,pcDbfieldName,pcformat)).
    end method.    
    
    method protected character CheckAndAddFieldToMap(pcname as character, pcMap as character):
        define variable hFld as handle no-undo.
        if lookup(pcname,pcMap) = 0 then 
        do:
            hFld = DataBuffer:buffer-field(pcname) no-error.
            if valid-handle(hFld) then
                pcMap = right-trim(subst("&1,&2.&1&3&4",pcname,BufferHandle:name,",",pcMap),","). 
                 
        end.
        return pcMap.
    end method.    
    
    method protected IConstantField DefineConstantField(pcDBFieldname as character, pcValue as character):
        if valid-handle(BufferHandle) then 
            return DefineConstantField(BufferHandle,pcDbFieldname,pcValue).
        else
            undo , throw new IllegalOperationError("DefineConstantField(DbFieldname,Value) called before BufferHandle is prepared").      
    end method.     
    
    method protected IConstantField DefineConstantField(pcDBFieldname as character, piValue as integer):
        if valid-handle(BufferHandle) then 
            return DefineConstantField(BufferHandle,pcDbFieldname,piValue).
        else
            undo , throw new IllegalOperationError("DefineConstantField(DbFieldname,Value) called before BufferHandle is prepared").     
    end method.     
    
    method protected IConstantField DefineConstantField(pcDBFieldname as character, plValue as logical):
        if valid-handle(BufferHandle) then 
            return DefineConstantField(BufferHandle,pcDbFieldname,plValue).
        else
            undo , throw new IllegalOperationError("DefineConstantField(DbFieldname,Value) called before BufferHandle is prepared").     
    end method.     
    
    
    method protected IConstantField DefineConstantField(phBuffer as handle,pcDBFieldname as character, pcValue as character):
        return cast(AddSourceField(new ConstantField(pcValue,phBuffer,pcDbFieldname)),IConstantField).
    end method.     
    
    method protected IConstantField DefineConstantField(phBuffer as handle,pcDBFieldname as character, piValue as integer):
        return cast(AddSourceField(new ConstantField(piValue,phBuffer,pcDbFieldname)),IConstantField).
    end method.     
    
    method protected IConstantField DefineConstantField(phBuffer as handle,pcDBFieldname as character, plValue as logical):
        return cast(AddSourceField(new ConstantField(plValue,phBuffer,pcDbFieldname)),IConstantField).
    end method.     
    
    method protected ISourceField AddSourceField(pSourceField as ISourceField):
        if not valid-object(SourceFieldManager) then 
            SourceFieldManager = new SourceFieldManager(GetDataSourceKeys(BufferHandle)).
        return SourceFieldManager:Add(pSourceField).   
    end.    
    
    method protected IFieldProxy AddFieldProxy(pProxy as IFieldProxy):
        // set to blank since it is init ? in order to be used as except parameter in attach 
        if ProxyFieldNames = ? then 
            ProxyFieldNames = "".
        if lookup(pProxy:name,ProxyFieldNames) = 0 then
        do:  
            // see comments in property
            ProxyFieldNames = ProxyFieldNames + (if ProxyFieldNames = "" then "" else ",") + pProxy:Name.
            
            AddFieldProxyToCollection(pProxy).
        end.      
        
        return pProxy.  
    end.    
    
    // separared out to be overriden in classes that only needs the ProxyFieldNames
    // TODO thi is messy  
    method protected logical AddFieldProxyToCollection(pProxy as IFieldProxy):
        if not valid-object(ProxyManager) then 
        do:
            ProxyManager = new ProxyManager(BufferName,GetDataSourceKeys(BufferHandle)).
        end.
        return ProxyManager:Add(pProxy).    
    end.    
      
    // allow subclasses to override and do nothing 
    method protected void HandleQueryError (pRequest as IGetDataTableRequest, pError as Error):
        undo, throw new ApplicationError(subst("Failed to build database query from query '&1'",pRequest:QueryString),pError).  
    end method.
    
    // allow subclasses to override and do nothing 
    method protected void HandleInnerJoinQueryError (pRequest as IGetDataTableRequest, pError as Error):
        HandleQueryError(pRequest,pError).  
    end method.
    
    // separated out to allow override for cases where there are more than one unique key and this need to be controlled 
    method protected Pmfo.Core.BusinessLogic.IGetDataTableRequest GetFindCurrentRowRequest(phBuffer as handle):
        return QueryUtil:GetFindCurrentRowRequest(phBuffer).
    end method.
     
    // called from refreshBuffer - in order to use mapping and after row when refreshing data 
    method public IGetDataTableRequest PrepareCurrent(phBuffer as handle, pParentRequest as IGetDataTableRequest).
        define variable oTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        oTableRequest = GetFindCurrentRowRequest(phBuffer).
        Prepare(phBuffer,oTableRequest,pParentRequest).
        return oTableRequest.
    end method.
    
    method final public logical PrePrepare(phBuffer as handle, pRequest as IGetDataTableRequest).
        return prepare(phBuffer,pRequest,?,true). 
    end method.
    
    method protected character GetParentNames(phBuffer as handle):
        return GetParentNames(phbuffer,?). 
    end method.      
    
    method protected character GetParentNames(phBuffer as handle, phTopBuffer as handle):
        define variable cNames as character no-undo.
        if valid-handle(phBuffer:parent-relation) then 
        do: 
            if valid-handle(phTopBuffer) = false or phBuffer:parent-relation:parent-buffer <> phTopBuffer then
                cNames = GetParentNames(phBuffer:parent-relation:parent-buffer,phTopBuffer).
            cNames = cNames 
                + (if cNames > "" then "," else "")
                + phBuffer:parent-relation:parent-buffer:name.
                 
        end.   
        return cNames.
    end method.  
    
    // hook to allow a final prepare when the query is fully prepared 
    // The realted datasources  QueryStringWithoutParent is assigned at this point and can be usyed if necessary
    method public void PrepareInnerJoinRequest(pRequest as IGetDataTableRequest, pcInnerJoinTables as character, phdataset as handle):
    end method.     
    
    method final public logical PrepareInnerJoins(pRequest as IGetDataTableRequest, pcInnerJoinTables as character,  pParentRequest as IGetDataTableRequest, phdataset as handle):
        define variable iTempTable        as integer no-undo.
        define variable hTempTableBuffers as handle  extent no-undo.
        define variable oDataSourceMap    as IHandleDataSourceMap no-undo.
        define variable oDataSources      as IDataSource extent no-undo.
        define variable oHandles          as ICollection no-undo.
        define variable cName             as character no-undo. 
        define variable cBufferName       as character no-undo. 
        define variable iParentTable      as integer no-undo.
        define variable oTableBufferMap   as IStringStringMap no-undo. 
        define variable hBuffer as handle no-undo.
        define variable iRename as integer no-undo.
        define variable iDbBuffer as integer no-undo.
        define variable cOtherTables as character no-undo.  
        define variable cAllJoinedTables as character no-undo.  
        define variable cTableQueries     as character extent no-undo.
        define variable iExpression as integer no-undo.
        define variable cExpression as character no-undo.
        define variable cInnerJoinQuery as character no-undo. 
        define variable hNewHandles       as handle extent no-undo.
        define variable oIterator         as IIterator no-undo.
        define variable hBuffers          as handle no-undo.
        define variable cNewNames as character no-undo.
        define variable cNewWhere as character no-undo.   
        define variable oQueryBuilder as QueryBuilder no-undo.
        define variable oInnerJoinSourceMap as IDataSourceMap no-undo.
        define variable lMainbufferAdded as logical no-undo.
        define variable cQueryMode       as character no-undo.
        define variable hRelation        as handle no-undo.
        define variable iRelation        as integer no-undo.
        define variable lChildisparent   as logical no-undo.
        define variable cAddQuery        as character no-undo.
        define variable cRenamelist      as character no-undo.
        define variable iRenameentry     as integer no-undo.
        define variable oTableBufferRenameMap as IStringStringMap no-undo.     
        define variable iBufferHandle         as integer no-undo.  
        define variable iJoinTable            as integer no-undo.
        define variable lInnerJoinOptimized   as logical no-undo.
        define variable lMainJoinAdded        as logical no-undo.
        define variable cMainBaseQueries      as character extent no-undo.
        define variable cRecordPhrase              as character no-undo.
        define variable iStartMainOuterJoinTables  as integer no-undo.
        define variable iLastinnerJoinMainTable    as integer no-undo.
        define variable iBuffer                    as integer no-undo.
        define variable cJoinToChildAsParent       as character no-undo.
        define variable iSource                    as integer no-undo. 
        define variable oProxyManager              as ProxyManager no-undo.
      
        oDataSourceMap = GetDataSourceMap(phdataset).
     
        oHandles = new Collection().
      
        // referenced table parents must be included - this ensures the list is ordered 
        // with parents first 
        
        do iTempTable = 1 to num-entries(pcInnerJoinTables):
            hBuffer = phDataset:get-buffer-handle(entry(iTempTable,pcInnerJoinTables)).
            cOtherTables = GetParentNames(hBuffer,DataBuffer).
            do iParentTable = 1 to num-entries(cOtherTables):
                cBufferName = entry(iParentTable,cOtherTables). 
                if cBufferName <> Databuffer:name 
                    and lookup(cBufferName,cAllJoinedTables) = 0 then
                    cAllJoinedTables = cAllJoinedTables
                        + (if cAllJoinedTables > "" then "," else "") 
                        + cBufferName.
                               
            end.
            if lookup(hBuffer:name,cAllJoinedTables) = 0 then
                cAllJoinedTables = cAllJoinedTables
                    + (if cAllJoinedTables > "" then "," else "") 
                    + hBuffer:name.
        end.
        
        oTableBufferMap = new StringStringMap(). 
        
        if valid-object(pParentRequest) then
        do: 
            extent(hTempTablebuffers) = num-entries(cAllJoinedTables) + 1.
            hTempTableBuffers[1] = Databuffer.
            do iTempTable = 1 to num-entries(cAllJoinedTables):
                hTempTableBuffers[iTempTable + 1] = phdataset:get-buffer-handle(entry(iTemptable,cAllJoinedTables)).
            end.    
        end. 
        else 
            hTempTableBuffers = OrderBuffersForInnerJoin(cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest), cAllJoinedTables, phDataset).
        
        lInnerJoinOptimized = hTempTableBuffers[1] <> DataBuffer.
         
        extent(oDataSources) = extent(hTempTableBuffers).
        
        do iTempTable = 1 to extent(hTempTableBuffers): 
            if hTempTablebuffers[iTempTable] <> Databuffer then 
            do:
                oDataSources[iTempTable] = oDataSourceMap:Get(hTempTableBuffers[iTempTable]).
                extent(cTableQueries) = ?.
                cTableQueries = QueryUtil:SplitQuery(oDataSources[iTempTable]:QueryStringWithoutParent).
                cRenameList = "".
                iRename  = 0.
                do iDbBuffer = 1 to extent(cTableQueries):
                    cBuffername = "".
                    cAddQuery = "". 
                    // get rid of record phrase 
                    cTableQueries[iDbBuffer] = QueryUtil:StripQueryRecordphrase(cTableQueries[iDbBuffer]).
                    if iDbBuffer = 1 then 
                    do: 
                        if lMainbufferAdded then
                            cAddQuery =  replace(substr(trim(cTableQueries[iDbBuffer]),4)," each "," first ").
                        else do:
                            cAddQuery = trim(cTableQueries[iDbBuffer]).
                            if iTempTable > 1 then  
                                cAddQuery = substr(trim(cAddQuery),4).
                        end.    
                        cBufferName = oDataSources[iTempTable]:BufferHandles[iDbBuffer]:name.
                    end.
                    else if StringUtil:FindAnyWord(cTableQueries[iDbBuffer],"outer-join") = 0 then
                    do:
                        cAddQuery = cTableQueries[iDbBuffer].
                        cBufferName = oDataSources[iTempTable]:BufferHandles[iDbBuffer]:name.
                    end.
                     
                    if cBufferName > "" then 
                    do:
                        if iDBBuffer > 1 and cRenameList > "" then 
                        do:
                            // subsequent tables is likely to have joins to the table that was renamed
                            do iRenameEntry = 1 to num-entries(cRenameList) by 2.
                                cAddQuery = QueryUtil:ReplaceTableName(cAddQuery, entry(iRenameentry,cRenameList), entry(iRenameentry + 1,cRenameList)).
                            end.    
                        end.    
                        cName = cBufferName.
                        /*                        if lookup(cName,BufferNames) > 0 then*/
                        /*                        do:                                  */
                        /*                                                             */
                        /*                                                             */
                        /*                                                             */
                        /*                        end.                                 */
                        
                        do while lookup(cName,BufferNames) > 0 or lookup(cName,cNewNames) > 0:
                            iRename = iRename + 1.
                            cName = subst("&1_&2",cBufferName,iRename).
                        end.
                        if cname <> cBufferName then 
                        do:
                            cAddQuery = QueryUtil:ReplaceTableName(cAddQuery, cBufferName, cName).
                            cRenameList = cRenameList 
                                + (if cRenameList = "" then "" else ",")
                                + cBufferName 
                                + ","
                                + cName.   
                            
                        end.
                         
                        if cAddQuery > "" then 
                            cInnerJoinQuery = left-trim(subst("&1, &2",cInnerJoinQuery,cAddQuery)," ,").
                        
                        create buffer hBuffer for table oDataSources[iTempTable]:BufferHandles[iDBbuffer]:table buffer-name cname.
                        
                        oHandles:Add(new WidgetHandle(hBuffer)).
                                        
                        if iDbBuffer = 1 then
                        do:
                            oTableBufferMap:put(hTempTablebuffers[iTempTable]:name,cName).
                        end.
                             
                    end. 
                    
                end. 
                if cRenameList > "" then 
                do:
                    if not valid-object(oTableBufferRenameMap) then 
                        oTableBufferRenameMap = new StringStringMap().
                    oTableBufferRenameMap:put(hTempTablebuffers[iTempTable]:name,cRenameList).
                end.  
            end.
            else do:
                iStartMainOuterJoinTables = 0.
                // split the base query in order to add all outer-joins at the end of the query
                cMainBaseQueries = QueryUtil:SplitQuery(QueryUtil:StripQueryRecordphrase(BaseQuery, output cRecordPhrase)).
                
                bufferhandleloop:
                do iBufferHandle = 1 to extent(BufferHandles):
                    if BufferHandles[iBufferHandle] = BufferHandle then
                        lMainbufferAdded  = true.
                    else if lMainbufferAdded then 
                    do:
                        // if pouter join then set iStartMainOuterJoinTables to use as start when adding these at end
                        if StringUtil:FindAnyWord(cMainBaseQueries[iBufferHandle],"outer-join") > 0 then
                        do: 
                            iStartMainOuterJoinTables = iBufferHandle.
                            leave bufferhandleloop.
                        end.    
                    end. 
                    oHandles:Add(new WidgetHandle(BufferHandles[iBufferHandle])).
                end.    
                // control the query loop for innerjoins  below
                if iStartMainOuterJoinTables > 0 then 
                    iLastinnerJoinMainTable = iStartMainOuterJoinTables - 1.
                else     
                    iLastinnerJoinMainTable = extent(BufferHandles).
                    
                if iTempTable > 1 then 
                do: 
                    
                    if entry(1,BufferNames) <> BufferName then 
                    do:
                        cJoinToChildAsParent = GetInnerJoinExpressionForOtherBuffer( oDataSources[1]:BufferHandles[1]:name,entry(1,BufferNames)) .
                      
                        if cjoinToChildAsParent = "" then
                        
                            // We can handle the changes to convert one table to each instead of first and add the databuffer as child, 
                            // but this becomes complicated when two tables are moved before the main table as we would need to join
                            // the two top tables to ensure the second has FIRST and a where clause that possibly joins them in a menaingful way
                            // (keep in mind that they may be the same physical table -  as of current ) 
                            // Whenever encountered the best fix is likely to ensure GetBufferHandles only moves bufferhandles when 
                            // there is no inner join pRequest:GetInnerJoins() = ""    
                            undo, throw new UnsupportedOperationError
                                (subst("The '&1' Entity Buffers and BaseQuery is run-time optimized to define '&2' as the first database buffer instead of the main '&3' database buffer."
                                + " This may conflict with the runtime optimization for the inner join for the expanded tables that has moved database buffer '&4' from the '&5' entity as the first buffer in the inner join query."
                                + " The Data Source does not support changes to both the default BaseQuery order and the default expand query order. Please Contact IT.",
                                entityname,
                                entry(1,BufferNames),
                                BufferName,
                                oDataSources[1]:BufferHandles[1]:name,
                                oDataSources[1]:entityname
                                )
                                ).
                    end.
                    // iLastinnerJoinMainTable is less than extent(Bufferhqandles) if we need to skip outerjoin tables here (will beadded afcter this)     
                    do iBuffer = 1 to iLastinnerJoinMainTable:
                        if iBuffer = 1 then
                            cInnerJoinQuery =  left-trim(subst("&1, &2",cInnerJoinQuery,left-trim(substr(cMainBaseQueries[iBuffer],4)))," ,").
                        else     
                            cInnerJoinQuery =  left-trim(subst("&1, &2",cInnerJoinQuery,cMainBaseQueries[iBuffer])).
                    end.
                end.
                else do:
                    // iLastinnerJoinMainTable is less than extent(Bufferhqandles) if we need to skip outerjoin tables here (will beadded afcter this)     
                    do iBuffer = 1 to iLastinnerJoinMainTable:
                        if iBuffer = 1 then
                            cInnerJoinQuery =  left-trim(subst("&1, &2",cInnerJoinQuery,cMainBaseQueries[iBuffer])," ,").
                        else     
                            cInnerJoinQuery =  left-trim(subst("&1, &2",cInnerJoinQuery,cMainBaseQueries[iBuffer])).   
                    end.
                end.
                
            end.    
        end. 
        // add any outer joins found at end
        if iStartMainOuterJoinTables > 0 then
        do: 
            do iBufferHandle = iStartMainOuterJoinTables to extent(BufferHandles):
                oHandles:Add(new WidgetHandle(BufferHandles[iBufferHandle])).
                cInnerJoinQuery =  left-trim(subst("&1, &2",cInnerJoinQuery,cMainBaseQueries[iBufferHandle])).   
            end.
        end.
    
        lMainJoinAdded =  hTempTablebuffers[1] = DataBuffer.
         
        // add joins to innerjoin buffers   
        do iTempTable = 2 to extent(hTempTableBuffers): 
            hRelation = ?.
            lChildisparent = false.
          
            if hTempTablebuffers[iTempTable] = DataBuffer then
            do:
                if cJoinToChildAsParent > "" then 
                do:  
                    cInnerJoinQuery = QueryUtil:InsertQueryExpression(
                        BufferHandles[1]:Name,
                        cInnerJoinQuery, 
                        cJoinToChildAsParent).
                    
                end.    
                else do:
                    relationloop:
                    do iRelation = 1 to Databuffer:num-child-relations:
                        do iJoinTable = 1 to ITempTable - 1:
                             
                            if DataBuffer:get-child-relation(iRelation):child-buffer = hTempTablebuffers[iJoinTable] then
                            do:
                                hRelation = DataBuffer:get-child-relation(iRelation).
                                lChildisparent = true.
                                leave.
                            end.  
                        end.  
                    end.
                  
                    if lChildIsParent then 
                    do:
                        cNewWhere = GetInnerJoinExpression(oDataSourceMap,hRelation, oTableBufferMap,true).
                        
                        cInnerJoinQuery = QueryUtil:InsertQueryExpression(
                            BufferName,
                            cInnerJoinQuery, 
                            cNewWhere).
            
                    end.   
                end.
                lMainJoinAdded = true.
            end.
            else do:
                if lMainJoinAdded =  false then
                do:
                    relationloop: 
                    do iRelation = 1 to hTempTablebuffers[iTempTable]:num-child-relations:
                        hRelation  = hTempTablebuffers[iTempTable]:get-child-relation (iRelation).
                        if hRelation:child-buffer =  hTempTablebuffers[iTempTable - 1] then
                        do:
                            cNewWhere = GetInnerJoinExpression(oDataSourceMap,hRelation, oTableBufferMap,true).
                            cInnerJoinQuery = QueryUtil:InsertQueryExpression(
                                string(oTableBufferMap:Get(hTempTableBuffers[iTemptable]:name)),
                                cInnerJoinQuery, 
                                cNewWhere).
                            leave relationloop.                  
                        end.               
                    end.                     
                end.
                else do:
                    hRelation = hTempTableBuffers[iTemptable]:parent-relation.
                    cNewWhere = GetInnerJoinExpression(oDataSourceMap,hRelation, oTableBufferMap,false).
         
                    cInnerJoinQuery = QueryUtil:InsertQueryExpression(
                        string(oTableBufferMap:Get(hTempTableBuffers[iTemptable]:name)),
                                        
                        cInnerJoinQuery, 
                        cNewWhere).
                end.  
                                       
            end.
        end.   
         
        // recreate the main datasource properties like BufferHandles, QueryHandle, DataSourcehnadle and 
        // attach the Databuffer  datasource 
        extent(hNewHandles) = oHandles:size.
        oIterator = oHandles:Iterator().
        iDbbuffer = 0.
        do while oIterator:HasNext(): 
            iDbBuffer = iDbBuffer + 1.
            hNewHandles[iDbBuffer] = cast(oIterator:Next(),WidgetHandle):value.
        end.    
        extent(BufferHandles) = ?.
        BufferHandles = hNewHandles.
                                                  
        CreateQuery().
         
        DataBuffer:attach-data-source(DataSourceHandle,FieldMap).
        
        do iSource = 1 to extent(oDataSources):
            if valid-object(oDataSources[iSource]) then
                oDataSources[iSource]:PrepareInnerJoinRequest(pRequest, pcInnerJoinTables, phdataset).
        end.
            
        // allow the instance to utilize the other sources to add or remove stugff from te final request 
        // NOTE the innerjoinquery cannot be changed only the request - before it is applied below  
        PrepareInnerJoinRequest(pRequest, pcInnerJoinTables, phdataset).
        
        // build the query from the request using the DataSourceInnerJoinMap wwhich resolves and redirects 
        // innerjoin qualified fields to the respective DataSource dealing with possible renaming   
        do on error undo, throw :
            oInnerJoinSourceMap = new DataSourceInnerJoinMap(this-object,oDataSourceMap,oTableBufferMap,oTableBufferRenameMap,phdataset).
            oQueryBuilder = new QueryBuilder(oInnerJoinSourceMap,BufferNames,cInnerJoinQuery).
            cInnerJoinQuery = oQueryBuilder:Build(pRequest,DataBuffer).
            catch e as Progress.Lang.Error :
                // default to call  HandleQueryError, whihc defaults to throw
                // overrides may ignore - and just do nothing to make this return false 
                HandleInnerJoinQueryError(pRequest,e).
                return false. 
            end catch.
        end.
       
        if valid-object(pRequest) and cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):Count then 
            cInnerJoinQuery = PrepareOrExecuteCount(cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest), input cInnerJoinQuery, false).
        
        PreparePaging(pRequest, oQueryBuilder).
        if cRecordPhrase > "" then 
            cInnerJoinQuery = subst("&1 &2",cInnerJoinQuery,trim(cRecordPhrase)).
            
        if valid-object(pParentRequest) then    
            cInnerJoinQuery = AddParentJoinToQuery(cInnerJoinQuery).   
         
        QueryHandle:query-prepare(cInnerJoinQuery).
        
        return true. 
    end method.
    
    method protected void CompareQueries(pcBuffername as character, poSource as IDataSource):
        cast(poSource,DataSource):CompareQueries(BufferHandles[lookup(pcBufferName,Buffernames)],GetBaseQuery() ).
    end method.     
    
    method public void CompareQueries(phbuffer as handle, pcQuery as character):
    /*        message                                         */
    /*          "buffer name"                                 */
    /*                                                        */
    /*        phbuffer:name "buffer table" phBuffer:table skip*/
    /*        pcQuery skip                                    */
    /*                                                        */
    /*         "entity" entityname  skip                      */
    /*        "basequery" GetBaseQuery()  skip                */
    /*                                                        */
    /*        //skip posource:GetBaseQuery()                  */
    /*                                                        */
    /*                                                        */
    /*        view-as alert-box.                              */
    /*                                                        */
        
    end method.     
    
    // final ? - as of current we only allow override of the first buffer, but this method is separate to allow thois to 
    // be overridable in the future 
    method protected handle extent OrderBuffersForInnerJoin(pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, pJoinTables as character, phDataset as handle):
        define variable hBuffers       as handle    extent no-undo.
        define variable i              as integer no-undo.
        define variable hBuffer        as handle no-undo.
        define variable iExtent        as integer no-undo.
        define variable hParent        as handle no-undo.
        define variable iParent        as integer no-undo.
        // add an extent since this-object DataBuffer is not in the list
        extent(hBuffers) = num-entries(pJoinTables) + 1.
        hBuffers[1] = GetFirstBufferForInnerJoin(pRequest, pJoinTables, phDataset).
        if not valid-handle(hBuffers[1]) then  
            undo, throw new IllegalStateError("GetFirstBufferForInnerJoin returned an invalid handle. Please Contact IT").
        
        if hBuffers[1] = Databuffer then
        do: 
            do i = 2 to extent(hBuffers):
                hBuffers[i] = phDataset:get-buffer-handle(entry(i - 1,pJoinTables)).
            end.    
        end.
        else do:
            
            if lookup(hBuffers[1]:name,pJoinTables) = 0 then  
                undo, throw new IllegalStateError(subst("GetFirstBufferForInnerJoin returned buffer &1 which is not one of the inner join buffers in the dataset. Please Contact IT",hBuffers[1]:name)).
            iExtent = 1.
            do while true on error undo, throw:
                iExtent = iExtent + 1. 
                if valid-handle(hBuffers[iExtent - 1]:parent-relation) then
                do:
                    hBuffers[iExtent] = hBuffers[iExtent - 1]:parent-relation:parent-buffer.
                    if hBuffers[iExtent] = DataBuffer then leave.
                end.
                else 
                    undo, throw new IllegalStateError(subst("Buffer &1 encountered while traversing parent relation ships for buffer returned from GetFirstBufferForInnerJoin. Please Contact IT",hBuffers[iExtent - 1]:name)).
            end.
            
            do i = iExtent to extent(hBuffers):
                hBuffer = phDataset:get-buffer-handle(entry(i - 1,pJoinTables)).
                
                if hBuffer <> hBuffers[1] then 
                do:  
                    iExtent = iExtent + 1.
                    hBuffers[iExtent] = hBuffer.
                end.
            end.
        end.
        return hBuffers.
         
    end method.     
    
    method public logical HasUniqueIndexFieldsForFirstTable(pcFieldList as character) :
        return HasUniqueIndexFieldsForFirstTable(pcFieldList,false).     
    end.    
     
    /* check if a field list has referecces to all componenets of a unique index in the first buffer in the query
       @Params - pcFieldList - list of temp-table fields from the IGetDataTableRequest - typically GetPredicateFieldEqualMatches
               pcQualifier - specifies the qualifier used for the field when applicable 
                            - for innerjoins this would be the temp-table name in the requested dataset
                             
    */   
    method public logical HasUniqueIndexFieldsForFirstTable(pcFieldList as character, plUseQualifier as logical) :
        define variable cUniqueIndexFields as character extent no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo. 
        define variable cDbField as character no-undo.
        define variable cField as character no-undo.
        define variable lFound as logical no-undo.
        
        if pcFieldList = "" then 
            return false. 
        
        cUniqueIndexFields = BufferUtil:GetFieldsInUniqueIndexes(BufferHandles[1], false).
        indexloop:
        do i = 1 to extent(cUniqueIndexFields):
            lFound = false.
            fieldloop:
            do j = 1 to num-entries(cUniqueIndexFields[i]):
                cDbField = entry(j,cUniqueIndexFields[i]).
                cField = SourceColumn(subst("&1.&2",BufferHandles[1]:name,cDbField)).
                // the value will be blank if the db field is not in mapped to the temp-table
                if cField > "" then 
                do:
                    // the field list does not have qualifiers for the main table in which case we pass false
                    // so remove the temp-table name from value returned from sourceColumn in that case  
                    if plUseQualifier = false then
                        cField = entry(2,cField,".").
                          
                    if lookup(cField,pcFieldList) > 0 then  
                        lFound = true. // don't leave - keep looping as all fields must be a match
                    else do:
                        lFound = false.
                        next indexloop.
                    end.
                end.  
                else do:
                    lFound = false.
                    next indexloop. 
                end.        
            end.
            if lFound then 
                return true.     
        end.                 
        return false.
    end.  
    
    method public logical HasIndexFieldForFirstTable(pcFieldList as character) :
        return HasIndexFieldForFirstTable(pcFieldList,false).
    end method.     
    
    method public logical HasIndexFieldForFirstTable(pcFieldList as character, plUseQualifier as logical) :
        define variable cIndexFields as character   no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo. 
        define variable cDbField as character no-undo.
        define variable cField as character no-undo.
        
        if pcFieldList = "" then 
            return false. 
        
        cIndexFields = BufferUtil:GetFirstFieldInIndexes(DataBuffer,false).
         
        indexloop:
        do i = 1 to num-entries(cIndexFields):
            cDbField = entry(i,cIndexFields).
            cField = SourceColumn(subst("&1.&2",BufferHandles[1]:name,cDbField)).
            
            // the value will be blank if the db field is not in mapped to the temp-table
            if cField > "" then 
            do:
                // the field list does not have qualifiers for the main table in which case we pass false
                // so remove the temp-table name from value returned from sourceColumn in that case  
                if plUseQualifier = false then
                    cField = entry(2,cField,".").
                      
                if lookup(cField,pcFieldList) > 0 then  
                    return true. 
                
            end.
        end.                 
        return false.
    end.
    
    // fields to use to check if this source's buffer shuld be first in the inner join 
    // allow subclasses to override (typcially to ignore some fields that does not provide a reasn to add this buffer first in the join
    method protected character GetPreferredEqualsMatchesForInnerJoin(pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest):
        return pRequest:GetPredicateFieldEqualMatches().
    end method.    
    
    // fields to use to check if this source's buffer shuld be first in the inner join 
    // allow subclasses to override (typcially to ignore some fields that does not provide a reasn to add this buffer first in the join
    method protected character GetPreferredRangeMatchesForInnerJoin(pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest):
        return pRequest:GetPredicateFieldRangeMatches().
    end method.
    
    method protected handle GetFirstBufferForInnerJoin(pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, pJoinTables as character, phDataset as handle):
        define variable cPredicateRangeMatches as character no-undo.
        define variable i                      as integer no-undo.
        define variable cField                 as character no-undo.
        define variable oDataSourceMap         as IHandleDataSourceMap no-undo.
        define variable oDataSource            as IDataSource no-undo.
        define variable cDbFirstBuffer         as character no-undo.
        define variable cMatches               as character no-undo. 
        // check if there are any equal matches for a unique index in the first buffer
        // if not do the same check in the other source and make this the top buffer if unqique matches are found  
        cMatches = GetPreferredEqualsMatchesForInnerJoin(pRequest).
         
        if cMatches > "" then
        do:
            // reference to outer join tables will have period
            if HasUniqueIndexFieldsForFirstTable(cMatches) = false then
            do: 
                if index(cMatches,".") > 0 then 
                do:
                    
                    oDataSourceMap = GetDataSourceMap(phdataset).
                    do i = 1 to num-entries(pJoinTables):
                        oDataSource = oDataSourceMap:Get(phDataset:get-buffer-handle(entry(i,pJoinTables))).
                        
                        if cast(oDataSource,DataSource):HasUniqueIndexFieldsForFirstTable(cMatches,true) then // true means look for qualified fields only   
                            return phDataset:get-buffer-handle(entry(i,pJoinTables)).        
                    end.    
                end.
            end.
            else 
                return DataBuffer.  
        end.
        
        // check if there are any matches for a leading index component
        // check both the equal matches retrieved above and  range matches
        cPredicateRangeMatches = GetPreferredRangeMatchesForInnerJoin(pRequest).
        if cPredicateRangeMatches > "" then
        do i = 1 to num-entries(cPredicateRangeMatches):
            cField = entry(i,cPredicateRangeMatches).
            if lookup(cField,cMatches) = 0 then 
                cMatches = cMatches 
                    + (if cMatches = "" then "" else ",")
                    + cField.   
        end.
        
        if cMatches > "" then 
        do: 
            // reference to outer join tables will have period
            if HasIndexFieldForFirstTable(cMatches) = false then
            do: 
                if index(cMatches,".") > 0 then 
                do:
                    if not valid-object(oDataSourceMap) then
                        oDataSourceMap = GetDataSourceMap(phdataset).
                    do i = 1 to num-entries(pJoinTables):
                        oDataSource = oDataSourceMap:Get(phDataset:get-buffer-handle(entry(i,pJoinTables))).
                        if cast(oDataSource,DataSource):HasIndexFieldForFirstTable(cMatches,true) then // true means look for qualified fields only   
                            return phDataset:get-buffer-handle(entry(i,pJoinTables)).        
                    end.    
                end.
            end.
            else 
                return DataBuffer.
        end.    
        
        return GetFirstBufferForInnerJoinForLocalQuery(pJoinTables, phDataset).  
         
    end method.     
    
    // sub classes can override to override if a different buffer should 
     // be first when there are no querypredicates that matches any indexes
    method protected handle GetFirstBufferForInnerJoinForLocalQuery(pJoinTables as character, phDataset as handle):
        return DataBuffer.
    end.     
    
    // allow source to provide an inner join for a case where a child has been moved before the main buffer
    //  AND the main gbuffer is not the first buffer in the BaseQuery  
    method protected character GetInnerJoinExpressionForOtherBuffer(pcParent as character, pcChild as character):
        return "".
    end method.
    
    method protected character GetInnerJoinExpression(poDataSourceMap as IHandleDataSourceMap, phRelation as handle, pTToDbmap as IStringStringMap, plParentIsChild as logical):
        define variable i                as integer no-undo.  
        define variable cValue           as character no-undo.
        define variable cChildTTField    as character no-undo.
        define variable cChildDbField    as character no-undo.
        define variable cParentTTField   as character no-undo.
        define variable cParentDbField   as character no-undo.
        define variable hChildbuffer     as handle no-undo.
        define variable cParentBuffername as character no-undo.
        define variable cChildBuffername as character no-undo.
        define variable oParentSource      as IDataSource  no-undo.
        define variable oChildSource       as IDataSource no-undo.
        define variable cExpression as character no-undo.
        define variable lParentIsChild as logical no-undo.
        
        if phRelation:parent-buffer = Databuffer then
        do: 
            cParentBufferName = BufferName.
            oParentSource = this-object.
        end.
        else do: 
            oParentSource = poDataSourceMap:Get(phRelation:parent-buffer).
            cParentBuffername = pTToDbmap:Get(phRelation:parent-buffer:name).
        end.    
      
        oChildSource = poDataSourceMap:Get(phRelation:child-buffer).
        cChildBuffername = pTToDbmap:Get(phRelation:child-buffer:name).
        
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cParentTTField  = entry(i,phRelation:relation-fields). 
            cChildTTField   = entry(i + 1,phRelation:relation-fields) .
            cParentDbField  = oParentSource:ColumnSource(cParentTTField). 
            cChildDbField   = oChildSource:ColumnSource(cChildTTField). 
            
            if oParentSource <> this-object and entry(1,cParentDbField,".") <> cParentBuffername then
                entry(1,cParentDbField,".") = cParentBuffername.
            if entry(1,cChildDbField,".") <> cChildBuffername then
                entry(1,cChildDbField,".") = cChildBuffername.
            if plParentIsChild then 
                cExpression = cExpression 
                    + (if cExpression = "" then "" else " and ")
                    + subst("&1 = &2",cParentDbField,cChildDbField). 
           
            else
                cExpression = cExpression 
                    + (if cExpression = "" then "" else " and ")
                    + subst("&1 = &2",cChildDbField,cParentDbField) . 
                        
        end. 
        return cExpression.
    end method.    
    
    method final public logical Prepare( phBuffer as handle,  pRequest as IGetDataTableRequest ):
        return prepare(phBuffer,pRequest,?). 
    end method.
    
    method final public logical Prepare(phBuffer as handle, pRequest as IGetDataTableRequest, pParentRequest as IGetDataTableRequest ):
        return prepare(phBuffer,pRequest,pParentRequest,false). 
    end method.    
    
    method protected void PreparePaging(pRequest as IGetDataTableRequest, pQueryBuilder as QueryBuilder) :
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable iSource as integer no-undo.
        
        if pRequest:NumRecords > 0 then 
        do:
            NumRecords = pRequest:NumRecords.
                //  if UseBeforeFillWithPaging = false then
                // we need to set this also when UseBeforeFillWithPaging = false 
                // otherwise AfterRow continiues to fill data (TODO research this)
            Databuffer:batch-size = pRequest:NumRecords.
        end.
                    
        if pRequest:Skip > 0 then
        do:        
            if pRequest:PagingContext > "" then
                undo, throw new BadRequestError("Skip and Paging are conflicting request properties").
            if UseBeforeFillWithPaging = true then
                RestartRow = pRequest:Skip + 1.
            else    
                DataSourceHandle:restart-row  = pRequest:Skip + 1.
        end.
        oRequest = cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest).
            
            // only Psi request supports paging context 
        if valid-object(oRequest) then
        do:
            // external paging is an option to use serialize-name for fields in context
            ExternalPaging = oRequest:ExternalPaging.
            oRequest = cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest).
            if valid-object(oRequest:PagingContextEntry) then
            do:
                // The RestartIds are defined as property to be used in BeforeFillWithPaging
                // but this is not implemented yet   
                if UseBeforeFillWithPaging then 
                    undo, throw new NotImplementedError(subst("Batching on many to one query in &1",Entityname)).
            
                RestartIds = GetPagingRowids(oRequest,pQueryBuilder).
                // no need to do this if the fill is done in BeforeFillWithPaging
                if UseBeforeFillWithPaging = false then
                do iSource = 1 to extent(RestartIds):       
                    DataSourceHandle:restart-rowid(iSource) = RestartIds[iSource].
                end.     
            end.
        end.    
        else if pRequest:PagingContext > "" then 
        do:
            undo, throw new UnsupportedOperationError(subst("Paging Context with &1",pRequest:GetClass():TypeName)).
        end.
         
    end method.    
        
     
    method final protected logical Prepare(phBuffer as handle, pRequest as IGetDataTableRequest, pParentRequest as IGetDataTableRequest, plSkipQueryPrepare as logical ):
       
        define variable oQueryBuilder as QueryBuilder no-undo.
        define variable cQuery        as character no-undo.
        define variable oRequest      as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable lOk           as logical no-undo.
        define variable lUniqueFind   as logical no-undo.
        define variable iIndexedPosition as integer no-undo.
        define variable oAppError        as AppError no-undo.
        define variable cChildJoinFields as character no-undo.
        define variable iFld             as integer no-undo.
        define variable lProxyJoin       as logical no-undo.
        define variable iRel             as integer no-undo.
        
        assign 
            NextRowKey = ?
            RestartRow = 0
            extent(RestartIds) = ?
            .
        
        DataBuffer = phBuffer.
        
        // should possibly be in initialize? but trying ot avoid depending on initialize   
        DefineFieldProxies().
         
        PrepareBuffer(phBuffer).
        
        if valid-object (ProxyManager) then 
            ProxyManager:Prepare(Databuffer, BufferHandle).
        
        // set to true in empty constructor  
        if NoConnectNeeded then 
            return true.
            
        extent(BufferHandles) = ?.
        // if valid-object(oRequest) is used in subsequen logic
        if(type-of(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest)) then
        do:
            oRequest = cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest). 
            BufferHandles = GetBufferHandles(oRequest, cast(pParentRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest)).
        end.
        else // wasted too much time debugging cases where the Ccs version was used by accident - there is no point supporting that 
            undo, throw new UnsupportedOperationError(subst("Unsupported TableRequest Type &1",pRequest:GetCLass():TypeName)).
        
        if extent(BufferHandles) = ? then
            undo, throw new IllegalStateError(subst("&1.GetBufferHandles() does not return any handles",getClass():TypeName)).
        
        // set to true to defi8ne query later (typically in a before-Fill event)  
        if NoQueryCreate then 
            return true.
        
        if UseBeforeFillWithPaging then
        do: 
            // NOTe that we thow unsupported operation if Count and queryontext is encountered further down 
            // for UseBeforeFillWithPaging
            DataBuffer:set-callback("before-fill","BeforeFillWithPaging").
            if valid-object(oRequest:QueryDefinition) then 
            do:
                if extent(oRequest:QueryDefinition:QuerySort) <> ? then            
                    DataSort = QueryUtil:GetQuerySortAsString(oRequest:QueryDefinition:QuerySort). 
            end.       
        end. 
        
        CreateQuery().   
      
        if ProxyFieldNames > "" and valid-object(pParentRequest) then
        do:   
            cChildJoinFields = BufferUtil:GetChildJoinFields(DataBuffer:parent-relation).     
            do iFld =  1 to num-entries(cChildJoinFields): 
                if lookup(entry(iFld,cChildJoinFields),ProxyFieldNames) > 0 then 
                do: 
                    lProxyJoin = true.
                    leave.   
                end.    
            end.    
        end.
        
        if(type-of(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest)) then
            BaseQuery = PrepareBaseQuery(BaseQuery, oRequest).
        cQuery = BaseQuery.
        
        if lProxyJoin then
        do: 
            if UseBeforeFillWithPaging then 
                undo, throw new NotImplementedError("UseBeforeFillWithPaging in DataSource with ProxyFieldNames").
            // TODO - paging with no attached data source ?
            DataBuffer:set-callback("before-fill","BeforeFillForProxyJoin").
        end.
        else do: 
         
            DataBuffer:attach-data-source(DataSourceHandle,FieldMap).
        /** Cannot do this here since we need to build the query to support child filter in innerjoin
        if valid-object(pParentRequest) then
        do:
            QueryStringWithoutParent = cQuery. // used by prepareInnerJoins
            cQuery = AddParentJoinToQuery(cQuery).
        end.     
        **/
        end.
                
        if plSkipQueryPrepare = false and valid-object(pRequest:QueryDefinition) then
        do on error undo, throw :
             
            oQueryBuilder = new QueryBuilder(this-object,BufferNames,cQuery).
            
            cQuery = oQueryBuilder:Build(pRequest,DataBuffer).
            
            if valid-object(pParentRequest) and not lProxyJoin then
            do:
                QueryStringWithoutParent = cQuery. // used by prepareInnerJoins
                cQuery = AddParentJoinToQuery(cQuery).
            end.    
            
            catch e as Progress.Lang.Error :
              
               // default to throw but overrides may ignore - and just return false to silently set no-fill
                HandleQueryError(pRequest,e).
                return false. 
            end catch.
        end.
        else if not lProxyJoin then
        do: 
            if valid-object(pParentRequest) then
            do:
                QueryStringWithoutParent = cQuery. // used by prepareInnerJoins
                cQuery = AddParentJoinToQuery(cQuery).
            end.    
        end.
        
        if  pRequest:NumRecords = 0 
            and pRequest:PagingContext = ""
            and pRequest:Skip = 0 then
        do:
            iIndexedPosition = StringUtil:FindAnyWord(cQuery,"indexed-reposition").
            if iIndexedPosition > 0 then 
               // remove 1 extra since there is space before and after
                substring(cQuery,iIndexedPosition,length("indexed-reposition") + 1) = "".
        
            QueryHandle:forward-only = true.
        end.
        
        // only Psi request 
        if valid-object(oRequest) then 
        do:
            lUniqueFind = oRequest:FindUnique.
        end. 
           
        if lUniqueFind = false and plSkipQueryPrepare = false then
        do:
            // only Psi request supports count
            if valid-object(oRequest) and cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):Count  then 
                cQuery = PrepareOrExecuteCount(cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest), input cQuery, valid-object(pParentRequest)).
            
            PreparePaging(pRequest, oQueryBuilder).
        end.
        if plSkipQueryPrepare = false then
        do:          
            /*            if entityname= "partners" then*/
            /*                message cQuery            */
            /*                view-as alert-box.        */
            lok = QueryHandle:query-prepare(cQuery).
            
            // workaround for recursive data source fill-where-string and query prepare-string not being in sync
            // This is only for recursive cases where there are two tables and the child (this buffer) has a recursive relation to the parent 
            // and is a problem when the fill recurses to the parent and back to this and not for the first fill from the parent 
            // NOTE: We probably NEED this behavior for self recursisve relations in order to filter on top level only 
            //       but we could possibly use fill-where-string if we ever wanted to filer on children only.       
            if valid-handle(phBuffer:parent-relation) then
            do iRel = 1 to phBuffer:num-child-relations:
                if phBuffer:get-child-relation (iRel):recursive then 
                do: 
                    if phBuffer:parent-relation:parent-buffer = phBuffer:get-child-relation (iRel):child-buffer then
                    do:   
                        DataSourceHandle:fill-where-string = substr(QueryHandle:prepare-string,index(QueryHandle:prepare-string," where ") + 1).
                        leave.
                    end.    
                end.
            end.
        end.          
        else 
            lok = true. // not sure what is best here ???    
     
        RecordCounter = 0. 
        return lok. 
        catch e as Progress.Lang.Error :
            // @TODO should ideally check if production and only show the query return if not
            //  (ideally this error should never happen in production... but it will 
            if e:GetMessageNum(1) = 7328  or e:GetMessageNum(1) = 7325 or e:GetMessageNum(1) = 247 then
            do:
                oAppError = new ApplicationError("The query does not compile.",e). 
                oAppError:AddMessage(cQuery,?).
                undo, throw oAppError.
            end.    
            undo, throw e.
        end catch.   
    end method.
    
    method protected character PrepareOrExecuteCount(pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, pcQuery as character, plHasParent as logical) :
        define variable cCountQuery as character no-undo.
        Count = pRequest:Count.
                
        // if there is a sort do the count without the sort 
        // if we have a parent the parent temp-table is referneced in the query
        // and it wontwotk here   
        if Count then 
        do: 
            //  count if manual query is not implemented yet   
            if UseBeforeFillWithPaging then 
                undo, throw new NotImplementedError(subst("Count of many to one query of '&1'",Entityname)).
                   
            if valid-object(pRequest:QueryDefinition) 
                and extent(pRequest:QueryDefinition:QuerySort) > 0 
                and plHasParent = false then
            do:
                cCountQuery = trim(pcQuery).
                entry(1,cCountQuery,StringConstant:SPACE) = "preselect".
                // remove sort - not needed for count and may improve performance 
                cCountQuery = QueryUtil:RemoveSort(cCountQuery).
                QueryHandle:query-prepare(cCountQuery).
                QueryHandle:query-open ().
                TotalCount = QueryHandle:num-results.
                
            end.
            else do: 
                // The rule for child tables is that count and paging
                // only is set when the parents are single unique find records 
                // otherwise we would need to manage count for each parent 
                pcQuery = trim(pcQuery).
                entry(1,pcQuery,StringConstant:SPACE) = "preselect".
            end.
         
        end.
        return pcQuery. 
    end method.  
    
    method protected character GetNextRowKey(phSourceBuffer as handle,pRowid as rowid): 
          
        
        define variable iField          as integer   no-undo.
        
        define variable hFld            as handle    no-undo.
        define variable iLookup         as integer   no-undo.
        define variable cTTField        as character no-undo.
        define variable cDbField        as character no-undo.
        define variable cDbFieldTable   as character no-undo.
        define variable cDbFieldName    as character no-undo.
        define variable cValue          as character no-undo.
        define variable cNextRowKey     as character no-undo.
        define variable cNextValue      as character no-undo.
        define variable lHasSpaceOrPlus as logical   no-undo.
        define variable lUseFieldname   as logical   no-undo.
        define variable cKeys           as character no-undo.
        define variable dDate           as date      no-undo.
        
        cKeys = ServiceInfo:GetKeys(DataBuffer).
        if ckeys = "" then
            undo, throw new ApplicationError(subst("Cannot generate next row key for &1 with no unique key",DataBuffer:name)).               
       
        phSourceBuffer:find-by-rowid (pRowid,no-lock).
        
        if phSourceBuffer:avail then 
        do:
            do iField = 1 to num-entries(cKeys).
                cTTField = entry(iField,cKeys).
                iLookup = lookup(subst("&1.&2",DataBuffer:name,cTTField),DataBuffer:data-source-complete-map).
                cDbField = entry(iLookup + 1,DataBuffer:data-source-complete-map).
                cDbFieldTable = entry(1,cDbField,".").
                cDbFieldName = entry(2,cDbField,".").
                
                if cDbFieldTable = phSourceBuffer:name then 
                do:
                    assign
                        hFld  = phSourceBuffer:buffer-field(cDbFieldName).
                    if hFld:data-type begins "date" then
                    do:
                        if ExternalPaging then
                        do:
                            cNextValue = quoter(iso-date(hFld:buffer-value),"'").
                        end.
                        else
                            cNextValue  = quoter(hFld:buffer-value,"'").     
                    end.
                    else if (hfld:data-type = "character" or hFld:data-type = "longchar") then
                    do:
                        assign
                            cNextValue = hFld:buffer-value
                            lHasSpaceOrPlus = index(cNextValue,StringConstant:SPACE) > 0 or index(cNextValue,"+") > 0
                            cNextValue      = quoter(cNextValue,"'")
                            .
                    end.        
                    else
                        cNextValue = hFld:buffer-value.
                   
                    /* if hasspaceorplus use field name in expression so we can safely decode bad encoders like certain REST testing tools...
                       some REST tools will encode space as +, but not if there already is a + 
                       this is indeterministic since a value with + and no space could origin from both space and +
                    */     
                    if num-entries(cKeys) = 1 and not lHasSpaceOrPlus then 
                    do:
                        cNextRowKey = cNextValue.
                    end.
                    else do:
                        cNextRowKey = cNextRowKey 
                            + (if cNextRowKey = "" then "" else " and ")
                            + subst("&1 eq &2",
                            if ExternalPaging 
                            then DataBuffer:buffer-field(cTTField):serialize-name
                            else cTtField,
                            cNextValue
                            ).
                    end.                           
                end.     
            end.
        end.
        else 
            undo, throw new ApplicationError("Data source not available for next row key").               
        
        return cNextRowKey.
        
        catch e1 as ApplicationError :
            undo, throw e1.
        end.
        catch e2 as Progress.Lang.Error :
            undo, throw new ApplicationError("Could not identify next row key",e2).               
        end catch. 
    end method.         
     
    method protected rowid extent GetPagingRowids (pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, pQueryBuilder as QueryBuilder):
        define variable rIds          as rowid     extent no-undo.
        define variable cQuery        as character no-undo.
        define variable i             as integer   no-undo.
        define variable cErrorDetails as character no-undo. 
        
        if not valid-object(pQueryBuilder) then
            pQueryBuilder = new QueryBuilder(this-object,BufferNames,BaseQuery).
        
        extent(rids)= extent(BufferHandles).
        
        if extent(BufferHandles) = 1 then
        do:
            cQuery = pQueryBuilder:BuildFind(pRequest:PagingContextEntry,DataBuffer).
            BufferHandles[1]:find-unique(cQuery).
          
            if BufferHandles[1]:available then
                rids[1] = BufferHandles[1]:rowid. 
            else if BufferHandles[1]:ambiguous then
                undo, throw new ApplicationError(subst("Record &1 is ambiguous",BufferHandles[1]:name)).
            else 
                undo, throw new ApplicationError(subst("Record &1 not avail",BufferHandles[1]:name)). 
        end.    
        else do:
            cQuery = pQueryBuilder:Build(pRequest:PagingContextEntry,DataBuffer).
            QueryHandle:query-prepare(cQuery).
            QueryHandle:query-open().
            QueryHandle:get-first.
            do i = 1 to extent(BufferHandles):
                if BufferHandles[i] = bufferHandle then 
                do:
                    if BufferHandle:avail then
                    do: 
                        rids[i] = BufferHandles[i]:rowid.
                        leave.
                    end.
                    else 
                        undo, throw new ApplicationError(subst("Record &1 is not avail",BufferHandles[i]:name)).
                end.    
            end. 
            
            // There must only be one 
            QueryHandle:get-next.
            if BufferHandle:avail then 
                undo, throw new ApplicationError(subst("Record &1 is ambiguous",BufferHandle:name)).
        end.       
        return rids.
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) =  247 then 
                cErrorDetails = subst("' - &1'",cQuery).
            undo, throw new NotFoundError(subst("'&1' with paging context &2 &3",
                if pRequest:ExternalPaging then DataBuffer:serialize-name else DataBuffer:table-handle:name,
                pRequest:PagingContextEntry:ToString(),
                cErrorDetails 
                ),
                e      
                ).          
        end catch.
    end method.    
    
    method protected void SplitFieldMap(pcFieldMap as character, output pcFieldPairs as character extent, output pcExceptList as character extent):
        define variable cMaps as character extent no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable cTTField  as character no-undo.
        define variable cDbField  as character no-undo.
        define variable cDbTable  as character no-undo.
        define variable iTable    as integer no-undo.
        define variable iArrayPos as integer no-undo.
        extent(pcFieldPairs) = extent(BufferHandles).
        extent(pcExceptList) = extent(BufferHandles).
        if pcFieldMap > "" then
        do i = 1 to num-entries(pcFieldMap) by 2:
            assign
                cTTField = entry(i,pcFieldMap)
                cDbField = entry(i + 1,pcFieldMap)
                cDbTable = if num-entries(cDbField,".") = 2 then entry(1,cDbField,".") 
                           else if cDbField begins "rowid(" then StringUtil:ExtractParenthesis(cDbField,true) 
                           else BufferName
                iTable = lookup(cDbTable,BufferNames)
                pcFieldPairs[iTable] = pcFieldPairs[iTable] 
                              + (if pcFieldPairs[iTable] = "" then "" else ",")
                              + cTTField
                              + ","
                              + cDbField.
            // we must put all mapped fields in except list for other tables for buffer-copy to work like fill 
            exceptloop:
            do j = 1 to extent(pcExceptList):
                if j <> iTable then   
                do:
                    iArrayPos = index(cTtField,"[").  
                    if iArrayPos > 0 then
                    do:
                        cTTField = substring(cTTField,1,iArrayPos - 1).
                        if lookup(cTTField,pcExceptList[j]) > 0 then 
                            next exceptloop.
                    end.    
                    pcExceptList[j] = pcExceptList[j]
                        + (if pcExceptList[j] = "" then "" else ",") 
                        + cTtField.
                end.                                     
            end.     
        end. 
    end.    
    
    method protected void UpdateKey(phBuffer as handle, poNewKey as IKey ):
        define variable iInputKey     as integer no-undo.
        define variable iChild        as integer no-undo.
        
        define variable iField        as integer no-undo.
        define variable cParentField  as character no-undo.
        define variable hRelation     as handle  no-undo.
        define variable cParentFields as character no-undo.
        define variable iKeyField     as integer no-undo.
        define variable hQuery        as handle no-undo.
        define variable oChildkey     as IKey no-undo.
        define variable cJoin         as character no-undo.
        define variable oFieldValues  as IFieldValue extent no-undo.
        define variable iKey          as integer no-undo.
        define variable cChildName    as character no-undo.
        define variable cParentJoins  as character no-undo.
        define variable cChildJoins   as character no-undo.
        define variable lHaskey       as logical no-undo.
        define variable iKeyPos       as integer no-undo.
        define variable hField        as handle no-undo.
        define variable lAny          as logical no-undo.
        do iInputKey = 1 to extent(poNewKey:FieldValues): 
            hField = phBuffer:buffer-field(poNewKey:FieldValues[iInputKey]:name) no-error.
            if valid-handle(hField) then
                lany = true.
             
        end.
        if not lAny then 
            undo, throw new IllegalArgumentError(subst("UpdateKey called for buffer &1 with no fields that matches Key  &2",
                                                       phBuffer:name,
                                                       poNewKey:toString() 
                                                       )
                                                 ).
        
        do iChild = 1 to phBuffer:num-child-relations: 
            hRelation = phBuffer:get-child-relation(iChild).
            cParentFields = BufferUtil:GetParentJoinFields(hRelation).
            cJoin = BufferUtil:GetJoinExpression(hRelation).
            cParentJoins = BufferUtil:GetParentJoinFields(hRelation).
            cChildJoins  = BufferUtil:GetChildJoinFields(hRelation).
            
            extent(oFieldValues) = ?.
            extent(oFieldValues) = extent(poNewKey:FieldValues).
            lHaskey = false.
            do iKey = 1 to extent(oFieldValues): 
                iKeypos = lookup(poNewKey:FieldValues[iKey]:Name,cParentJoins). 
                if iKeyPos > 0 then 
                do:
                    oFieldValues[iKey] = new FieldValue(entry(iKeypos,cChildJoins),poNewKey:FieldValues[iKey]:Value).
                    lHaskey = true.
                end.
            end.
            if lHaskey then 
            do on error undo, throw: 
                oChildkey = new Key(ofieldValues).
                create query hQuery.
                hQuery:add-buffer(hRelation:child-buffer).
                hQuery:query-prepare(subst("preselect each &1 where &3 and row-state(&1) = &2",
                                            hRelation:child-buffer:name,
                                            row-created,
                                            cJoin)).
         
                hQuery:query-open().
                hQuery:get-first.
                do while hRelation:child-buffer:avail:
                    if hRelation:child-buffer:rejected = false and hRelation:child-buffer:error = false then
                    do: 
                        UpdateKey(hRelation:child-buffer,oChildkey).    
                    end.     
                    hQuery:get-next. 
                end. 
                finally:
                    if valid-handle(hQuery) then 
                        delete object hQuery.        
                end finally.
            end.
        end.  
        do iInputKey = 1 to extent(poNewKey:FieldValues): 
            hField = phBuffer:buffer-field(poNewKey:FieldValues[iInputKey]:name) no-error.
            if valid-handle(hField) then
            do:
                hField:buffer-value = poNewKey:FieldValues[iInputKey]:value:toString().
            end.
        end.       
    end method.
    
    method public logical FillRow(phBuffer as handle):
        return valid-object(FillRow(phBuffer,?)).
    end method. 
    
    method public IGetDataTableRequest FillRow(phBuffer as handle,pParentRequest as IGetDataTableRequest):
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest  no-undo.
        oRequest = QueryUtil:GetFindCurrentRowRequest(phBuffer).
        Prepare(phBuffer,oRequest, pParentRequest).        
        RefillCurrentRow().
        return oRequest.
    end method. 
    
    method public logical RefillCurrentRow().
        define variable hDs as handle.
        define variable i as integer no-undo.
        define variable lok as logical no-undo.   
        define variable cFieldMaps as character extent no-undo.
        define variable cExceptLists as character extent no-undo.
        
        if NoConnectNeeded or NoQueryCreate then
            // if no connect or no create needed in updatable dataource the refresh must be dealt with differently 
            // typically ensure all values are redresehed on save - i.e. call RowAfter and a add an empty override of RefillCurrentRow 
            // See ProcessControlGuideImageDS 
            undo, throw new IllegalStateError(subst("RefillCurrentRow called in DataSource with &1 set to true. Please Contact IT.",
                if NoConnectNeeded then "NoConnectNeeded" else "NoQueryCreate")
                ).
        QueryHandle:query-open().  
        QueryHandle:get-first(). 
        hds = DataBuffer:dataset.
        
        SplitFieldMap(FieldMap, output cFieldMaps,output cExceptLists).      
        do while QueryHandle:get-buffer-handle(1):avail:
            do i = 1 to QueryHandle:num-buffers:
                if QueryHandle:get-buffer-handle(i):avail then
                do:
                    DataBuffer:buffer-copy(QueryHandle:get-buffer-handle(i),cExceptLists[i],cFieldMaps[i]).
                end.    
            end.
          
            AfterRow(dataset-handle hds by-reference).
            lok = QueryHandle:get-next(). 
            if lok then 
            do:
                undo, throw new IllegalStateError("More than one row available for refill").
            end.    
            return true. 
        end.
        return false.  
    end method.
    
    method protected logical FillCurrentRow().
        define variable cFieldMaps   as character extent no-undo.
        define variable cExceptFields as character extent no-undo.
        SplitFieldMap(FieldMap, output cFieldMaps,output cExceptFields).
        return FillCurrentRow(cExceptFields,cFieldMaps).
    end method.    
    
    method protected logical FillCurrentRow(pcExceptLists as character extent,pcFieldMaps as character extent):
        define variable i as integer no-undo.
        define variable hDs as handle no-undo.
        define variable lok as logical no-undo.
        hDs = DataBuffer:dataset.
        do i = 1 to extent(BufferHandles):
            if BufferHandles[i]:avail then
                DataBuffer:buffer-copy(BufferHandles[i],pcExceptLists[i],pcFieldMaps[i]).
            lok = true.
        end.
        if lok then
            AfterRow(dataset-handle hds by-reference).
        return lok.    
    end method.    
    
    method protected logical HasCurrentRowChanged(pcFields as character).
        define variable hBefore as handle no-undo.
        hBefore = BufferUtil:AssertAndGetBeforebuffer(DataBuffer).
        return CompareBufferWithDb(hBefore,pcFields) > "".   
    end method.
    
    method protected logical HasCurrentRowChanged().
        return HasCurrentRowChanged("").   
    end method.
      
    // compare a temp-table buffer with the defined db buffers 
    // Assumes both tt and db buffer is avail !
    // Returns true if the buffers compares (are similar) 
    method protected character CompareBufferWithDb(phBuffer as handle):
        define variable hDummy1 as handle extent no-undo.
        define variable hDummy2 as handle extent no-undo.
        
        return CompareBufferWithDb(phBuffer,"",output hdummy1, output hDummy2). 
    end method.
    
    method protected character CompareBufferWithDb(phBuffer as handle, pcFields as character):
        define variable hDummy1 as handle extent no-undo.
        define variable hDummy2 as handle extent no-undo.
        
        return CompareBufferWithDb(phBuffer,pcfields,output hdummy1, output hDummy2). 
    end method.
    
    method protected character CompareBufferWithDb(phBuffer as handle,pcfields as character, output phDataFields as handle extent, output phSourceFields as handle extent ):
        define variable cFieldMaps     as character extent no-undo.
        define variable cExceptLists   as character extent no-undo.
        define variable i              as integer          no-undo.
        define variable cHiddenFields  as character no-undo.
        define variable cLobs          as character no-undo.
        define variable iLob           as integer no-undo.
        define variable cLobField      as character no-undo.
        define variable cChanges       as character no-undo.
        define variable cBufferChanges as character no-undo.
        define variable cProxyChanges  as character no-undo.
        define variable hBeforebuffer  as handle no-undo.
        define variable hDatafields    as handle extent no-undo.
        define variable hSourcefields  as handle extent no-undo.
        define variable hDatafieldsWork   as handle extent no-undo.
        define variable hSourcefieldswork as handle extent no-undo.
        define variable cMetricChanges   as character no-undo.
        
        if not phBuffer:avail then
            undo, throw new IllegalStateError(subst("Buffer '&1' is not available",phBuffer:name)).
      
        if not BufferHandle:avail then
            undo, throw new IllegalStateError(subst("Database table '&1' is not available",BufferHandle:name)).
        
        // @TODO - test and fix the following 
        // the field maps is likely not necessary since buffer compare uses the attach field map if 
        // one buffer is dataset buffer and the other is datasource buffer 
        
        SplitFieldMap(FieldMap, output cFieldMaps,output cExceptLists).        
        cHiddenFields = BufferUtil:GetHiddenFields(phBuffer).
        if NoLobs then 
            cLobs = GetLobFields().  
        
        do i = 1 to extent(BufferHandles):
            if BufferHandle = BufferHandles[i] then
            do: 
                cExceptLists[i] = trim(cExceptLists[i] + "," + cHiddenFields,",").
                if NoLobs and cLobs > "" then 
                do iLob = 1 to num-entries(cLobs): 
                    cLobField = entry(iLob,cLobs).
                    if lookup(cLobField,cExceptLists[i]) = 0 then
                        cExceptLists[i] = cExceptLists[i] 
                            + (if cExceptLists[i] = "" then "" else ",") 
                            + cLobField.
                end.
                     
                if phBuffer:buffer-compare(BufferHandle,"case-sensitive",? ,cFieldMaps[i]) = false then
                do:
                    extent(hDataFields) = ?.
                    extent(hSourceFields) = ?.
                    
                    // if sonething is different return the field names  
                    cBufferChanges = BufferUtil:CompareFields(phBuffer, BufferHandle, cExceptLists[i],pcfields, output hDataFields, output hSourceFields).
                     
                    if cBufferChanges > "" then
                    do:
                        cChanges = left-trim(subst("&1,&2",cChanges,cBufferChanges),",").
                        if extent(phdatafields) > 0 then
                        do:
                            assign
                                extent(hDataFieldsWork) = ?
                                extent(hSourceFieldsWork) = ?
                                hDataFieldsWork = phDataFields
                                hSourcefieldswork = phSourceFields
                                extent(phDataFields) = ?
                                extent(phSourceFields) = ?
                                phDataFields   = Array:mergeArrays(hDataFieldsWork, hDataFields)
                                phSourceFields = Array:mergeArrays(hSourceFieldsWork, hSourceFields)
                                .
                        end.
                        else 
                            assign                        
                                phDataFields = hDatafields
                                phSourceFields = hSourcefields
                                .
                    end.    
                end.
            end.    
        end.
        if valid-object(ProxyManager) then
        do:
             
            cProxyChanges = ProxyManager:CheckRow(this-object,phbuffer).
            if cProxyChanges > "" then 
            do:
                /*                if cChanges > "" and extent(hDataFields) > 1 then                                   */
                /*                do:                                                                                 */
                /*                        if extent(phdatafields) > 1 then                                            */
                /*                        do:                                                                         */
                /*                            assign                                                                  */
                /*                                extent(hDataFieldsWork) = ?                                         */
                /*                                extent(hSourceFieldsWork) = ?                                       */
                /*                                hDataFieldsWork = phDataFields                                      */
                /*                                hSourcefieldswork = phSourceFields                                  */
                /*                                extent(phDataFields) = ?                                            */
                /*                                extent(phSourceFields) = ?                                          */
                /*                                phDataFields   = Array:mergeArrays(hDataFieldsWork, hDataFields)    */
                /*                                phSourceFields = Array:mergeArrays(hSourceFieldsWork, hSourceFields)*/
                /*                               .                                                                    */
                /*                        end.                                                                        */
                /*                        else                                                                        */
                /*                            assign                                                                  */
                /*                                phDataFields = hDatafields                                          */
                /*                                phSourceFields = hSourcefields                                      */
                /*                                .                                                                   */
                /*                    end.                                                                            */
                
                cChanges = left-trim(subst("&1,&2",cChanges,cProxyChanges),",").
            end.     
        end.
        if valid-object(FieldSaveManager) then 
        do:
            
            cMetricChanges = FieldSaveManager:CheckRow(this-object, phbuffer).   
            if cMetricChanges > "" then 
                cChanges = left-trim(subst("&1,&2",cChanges,cMetricChanges),",").
              
        end.    
        return cChanges.  
    end method.
    
    method protected character GetLobFields ():
        define variable i       as integer no-undo.
        define variable cFields as character no-undo.
        define variable hFld    as handle no-undo. 
        do i = 1 to DataBuffer:num-fields:
            hFld = Databuffer:buffer-field(i).
            if hFld:data-type = "BLOB" or hFld:data-type = "CLOB" then 
                cFields = cFields 
                    + (if cFields = "" then "" else ",")
                    + hFld:name. 
        end.    
        return cFields.
        
    end method.
    
    // when the join is optimized to filrer  child parent the 
    // batch-size does not work well with fill-mnode 'merge' 
    method public void BeforeFillWithPaging( input dataset-handle phDataset ):
        define variable cPreparedQuery as character no-undo.
        define variable hQuery as handle no-undo.
        define variable i as integer no-undo.
        
        cPreparedQuery = QueryHandle:prepare-string.
                                                  
        QueryHandle:query-prepare (cPreparedQuery).
        
        this-object:Fill(NumRecords + if RestartRow > 0 then RestartRow - 1 else 0,RestartIds).
        if RestartRow > 0 then 
        do:
            create query hQuery.
            hQuery:add-buffer (Databuffer).
            hQuery:query-prepare(subst("preselect each &1 &2",DataBuffer:name,DataSort)).
            hQuery:query-open ().
            hQuery:get-first.
           
            do while true:
                i = i + 1.
                if RestartRow > 0 and DataBuffer:avail then
                    Databuffer:buffer-delete().
               
                if i < RestartRow - 1 then 
                    hQuery:get-next.               
                else do: 
                   //.if Count = false then
                    leave.
                      
                 //  Databuffer:buffer-delete().
                
                end.       
            end.   
            if Count then NumRecords = i.   
        end.  
        return no-apply.
        finally: 
            if valid-handle(hQuery) then 
                delete object hQuery.  
        end.     
    end method.
    
    
    method public void BeforeFillForProxyJoin( input dataset-handle phDataset ):
        define variable cPreparedQuery as character no-undo.
       
        cPreparedQuery = QueryHandle:prepare-string.
                                                  
        QueryHandle:query-prepare (AddParentJoinFieldsToQuery(dataBuffer:parent-relation,cPreparedQuery)).
        
        this-object:Fill().
        
        finally:
            QueryHandle:query-prepare(cPreparedQuery). 
        end.     
    end method.
    
    method public logical Fill().
        define variable rDummy as rowid extent no-undo.
        this-object:Fill (0,rDummy).
    end method. 
    
    // returns the row num that matces the start rowids to allow  
    // BeforeFillWithPaging get rid of the rows prior to this
    // 
    method protected int64 Fill(pagesize as int64, pStartIds as rowid extent).
        define variable hDs as handle.
        define variable i as integer no-undo.
        define variable cFieldMaps as character extent no-undo.
        define variable cExceptLists as character extent no-undo.
        define variable iChild as integer no-undo.
        define variable hChild as handle no-undo.
        define variable iStartRow as int64 no-undo. 
        define variable lFoundStart as logical init ? no-undo.
        QueryHandle:query-open().   
        QueryHandle:get-first(). 
        hds = DataBuffer:dataset.
        
        SplitFieldMap(FieldMap, output cFieldMaps,output cExceptLists).      
       
        do while QueryHandle:get-buffer-handle(1):avail:
 
           // if lSkip = false then
            do on error undo, throw:
                DataBuffer:buffer-create().
                /*                if iStartRow = 0 and extent(pStartIds) <> ?  then*/
                /*                   lFoundStart = true.                           */
                /*                                                                 */
                do i = 1 to  QueryHandle:num-buffers:
                    if QueryHandle:get-buffer-handle(i):avail then
                    do:
                        DataBuffer:buffer-copy(QueryHandle:get-buffer-handle(i),cExceptLists[i],cFieldMaps[i]).
                    /*                        if lFoundStart = true                                          */
                    /*                        and extent(pStartIds) <> ?                                     */
                    /*                        and pStartIds[i] <> QueryHandle:get-buffer-handle(i):rowid then*/
                    /*                           lFoundStart = false.                                        */
                       
                    end.    
                end.
                AfterRow(dataset-handle hds by-reference).
                
                /*                if lFoundStart then           */
                /*                    iStartRow = RecordCounter.*/
                
                do iChild = 1 to Databuffer:num-child-relations:
                    hChild = DataBuffer:get-child-relation (iChild):child-buffer .
                    hChild:fill().
                end.   
                 
                if pagesize > 0 then 
                do: 
                    if (extent(pStartIds) = ?  and RecordCounter > pageSize) then 
                    do: 
                        NextRowKey = GetNextRowKey(BufferHandle, BufferHandle:rowid).
                        DataBuffer:buffer-delete.
                        return 0.
                    end.  
                end.     
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 132  then
                    do: 
                       // ignore already exists if merge
                       // TODO check up front?? getuniqueindexes 
                       // - get value from db using field map
                       //   dataBuffer find-unique  
                        DataBuffer:buffer-delete.
                    end.   
                    else 
                        undo, throw e.
                end catch.
            end.
            QueryHandle:get-next(). 
            
        end.
        return 0.  
    end method.
    
    method protected character AddParentJoinFieldsToQuery(phRelation as handle,pcQuery as character):
        define variable i               as integer no-undo.
        define variable cNewWhere       as character no-undo.
        define variable cChild          as character no-undo.
        define variable cParent         as character no-undo.
        define variable cChildField     as character no-undo.
        define variable cSource         as character no-undo.
        define variable hParentField    as handle    no-undo.
        define variable cExpression     as character no-undo.
        define variable cAnd            as character no-undo.
        define variable oQueryPredicate as QueryPredicate no-undo.
        define variable oQueryEntry     as IQueryEntry  no-undo.
        define variable cQuery          as character no-undo.
        define variable cSourceBuffer   as character no-undo.
        
        cChild  = phRelation:child-buffer:name.
        // cAnd = " where ".
        
        do i = 1 to num-entries(phRelation:relation-fields) by 2.
            assign
                hParentField    = phRelation:parent-buffer:buffer-field(entry(i,phRelation:relation-fields))
                cChildField     = entry(i + 1,phRelation:relation-fields) 
                cSource         = columnSource(cChildField)
                cSourcebuffer   = entry(1,cSource,".")
                oQueryPredicate = new QueryPredicate(cSource,QueryOperatorEnum:Eq,hParentField:buffer-value)
                oQueryEntry     = ColumnExpression(oQueryPredicate)
                cNewWhere       = cNewWhere 
                                + cAnd 
                                + oQueryEntry:ToString()
                  
                cAnd             = " and ".               
        end. 
        cQuery = QueryUtil:InsertQueryExpression(BufferName, pcQuery, cNewWhere).
     
        return cQuery.
         
    end method.
    
    method protected character AddParentJoinFieldsToClientQuery(phRelation as handle,pQueryDefinition as IQueryDefinition):
        define variable i               as integer no-undo.
        define variable cChildField     as character no-undo.
        define variable hParentField    as handle    no-undo.
   
        define variable oQueryPredicate as QueryPredicate no-undo.
       // cAnd = " where ".
        do i = 1 to num-entries(phRelation:relation-fields) by 2.
            assign
                hParentField    = phRelation:parent-buffer:buffer-field(entry(i,phRelation:relation-fields))
                cChildField     = entry(i + 1,phRelation:relation-fields) 
                oQueryPredicate = new QueryPredicate(cChildField,QueryOperatorEnum:Eq,hParentField:buffer-value)
                .
            pQueryDefinition:AddEntry(oQueryPredicate).
                                
        end. 
         
    end method.
    
    method protected character AddParentJoinToQuery(pcQuery as character):
        define variable i            as integer no-undo.
        define variable cFillWhere   as character no-undo.
        define variable cFieldExp    as character no-undo.
        define variable cNewWhere    as character no-undo.
        define variable cChildField  as character no-undo.
        define variable cParentField as character no-undo.
        define variable cAnd         as character no-undo.
        define variable cChildBuffer as character no-undo.
        cFillWhere = left-trim(DataSourceHandle:fill-where-string).
        entry(1,cFillWhere,StringConstant:SPACE) = "".
        cFillWhere = left-trim(replace(cFillWhere," and "," ")).
        do i = 1 to num-entries(cFillWhere," ").
            cFieldExp    = entry(i,cFillWhere," ").
            cChildField  = entry(1,cFieldExp,"=").
            cChildbuffer = entry(1,cChildField,"."). 
            cParentField = entry(2,cFieldExp,"=").
            cNewWhere    = cNewWhere 
                + cAnd 
                + cChildField + " eq " + cParentField.
            cAnd         = " and ".               
        end. 
        if cChildbuffer <> BufferName and lookup(cChildbuffer,BufferNames) > 0 then
            return QueryUtil:InsertQueryExpression(cChildbuffer, pcQuery, cNewWhere).
        else 
            return QueryUtil:InsertQueryExpression(BufferName, pcQuery, cNewWhere).
    end method.
    
    /* TransactionManager will fire this if it finds an override in 
       the concrete class
       An error will be thrown if the CommitScopeEnum is not ALL or NESTED
       */ 
    method public void DatasetTransactionEnd( input dataset-handle phDataset ).
    end method. 
    
    method protected void BeginTransactionValidate(input dataset-handle phDataset):
    end method.   
    
    method protected void CreatePreTransValidate(input dataset-handle phDataset):
    end method.   
    
    method protected void CreateBeginTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void CreateEndTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void CreatePostTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void WritePreTransValidate(input dataset-handle phDataset):
    end method.    
   
    method protected void WriteBeginTransValidate(input dataset-handle phDataset):
    end method.    
   
    method protected void WriteEndTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void ActionEndTransValidate(pAction as IAction,input dataset-handle phDataset):
    end method.
    
    method protected void WritePostTransValidate(input dataset-handle phDataset):
    end method.    
   
    method protected void ActionPostTransValidate(pAction as IAction,input dataset-handle phDataset):
    end method.
   
    method protected void DeletePreTransValidate(input dataset-handle phDataset):
    end method. 
    
    method protected void DeleteBeginTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void DeleteEndTransValidate(input dataset-handle phDataset):
    end method.    
    
    method protected void DeletePostTransValidate(input dataset-handle phDataset):
    end method. 
    
    // optimisic locking support woth check sum (from Steve Southwell)
    method protected character GetRecordChecksum(
        input bh as handle
        ):
        
        define variable vraw as raw no-undo.
        define variable fh as handle no-undo.
        define variable hbtt as handle no-undo.
        
        if true then
            undo, throw new NotImplementedError().
        
        if not bh:available then undo, throw new Progress.Lang.AppError("Buffer is not available for checksum",-1).
        
       // uncomment the tt at top 
       //  hbtt = temp-table ttRawField:default-buffer-handle.
        hbtt:empty-temp-table().
        hbtt:buffer-create().
        fh = hbtt:buffer-field("rawField").
        bh:raw-transfer(true,fh).

        return encode(string(fh:buffer-value())).
        finally:
            if valid-object(fh) then delete object fh.
            if valid-object(hbtt) then delete object hbtt.
        end finally.
    end method.
    
    // future - support initialize values - added here since we generate code for this from SDOs
    method protected void GetNewRow(input dataset-handle phDataset):
    end method.
    
    method public void AfterRow(input dataset-handle phDataset):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo.
        
        RecordCounter = RecordCounter + 1.
        if valid-handle(SeqFieldHandle) then 
            SeqFieldHandle:buffer-value = RecordCounter.

        if Count and TotalCount  = ? then
        do:
            TotalCount = QueryHandle:num-results.
        end.
        if valid-object(ProxyManager) then 
            ProxyManager:AfterRow(BufferNames, BufferHandles).
                
    end method. 
   
    method protected void AssertHookEvent(pEventType as ValidationHookEventType):
        if transaction then 
        do:
            if ValidationHookEventType:Pre:Equals(pEventType) or ValidationHookEventType:Post:Equals(pEventType) then
                undo, throw new IllegalStateError(subst("Active transaction when processing '&1' validation hook event type",caps(pEventType:ToString()))).
        
        end.
        else  
        do:
            if ValidationHookEventType:Begin:Equals(pEventType) or ValidationHookEventType:End:Equals(pEventType) then
                undo, throw new IllegalStateError(subst("No transaction asctive when processing '&1' validation hook event type",caps(pEventType:ToString()))).
        
        end.
    end method.     
     
    method public void ValidateRowAction(phBuffer as handle, pAction as IAction, pEventType as ValidationHookEventType):
        define variable hDataset as handle no-undo.
       
        hDataset = phbuffer:dataset.
        AssertHookEvent(pEventType).
       
        case pEventType:
            when ValidationHookEventType:Begin 
            or when ValidationHookEventType:Pre then
                undo, throw new IllegalOperationError(subst("'&1' validation hook event type is not supported for Actions",caps(pEventType:ToString()))).
            
            when ValidationHookEventType:End then

                ActionEndTransValidate(pAction,input dataset-handle hDataset by-reference).
            when ValidationHookEventType:Post then
                ActionPostTransValidate(pAction,input dataset-handle hDataset by-reference).    
        end case.   
    end method.    
    
    method public void ValidateRow(phBeforeBuffer as handle, pEventType as ValidationHookEventType):
        define variable hDataset as handle no-undo.
        define variable hAfter   as handle no-undo.
        hDataset = phBeforebuffer:dataset.
        
        AssertHookEvent(pEventType).
        
        if phBeforeBuffer:row-state <> row-deleted then
        do:
            hAfter = phBeforeBuffer:after-buffer.
            hAfter:find-by-rowid(phBeforeBuffer:after-rowid).
        end.        
        
        case pEventType:
            when ValidationHookEventType:Begin then
                case phBeforeBuffer:row-state:
                    when row-deleted then
                        DeleteBeginTransValidate(input dataset-handle hDataset by-reference).
                    when row-modified then
                        WriteBeginTransValidate(input dataset-handle hDataset by-reference).
                    when row-created then
                        do:
                            CreateBeginTransValidate(input dataset-handle hDataset by-reference).
                            WriteBeginTransValidate(input dataset-handle hDataset by-reference).
                        end.
                end. // phBeforeBuffer:row-state 
            when ValidationHookEventType:End then
                case phBeforeBuffer:row-state:
                    when row-deleted then
                        DeleteEndTransValidate(input dataset-handle hDataset by-reference).
                    when row-modified then
                        WriteEndTransValidate(input dataset-handle hDataset by-reference).
                    when row-created then
                        do:
                            CreateEndTransValidate(input dataset-handle hDataset by-reference).
                            WriteEndTransValidate(input dataset-handle hDataset by-reference).
                        end.
                end. // phBeforeBuffer:row-state  
            when ValidationHookEventType:Pre then
                case phBeforeBuffer:row-state:
                    when row-deleted then
                        DeletePreTransValidate(input dataset-handle hDataset by-reference).
                    when row-modified then
                        WritePreTransValidate(input dataset-handle hDataset by-reference).
                    when row-created then
                        do:
                            CreatePreTransValidate(input dataset-handle hDataset by-reference).
                            WritePreTransValidate(input dataset-handle hDataset by-reference).
                        end.
                end.  // phBeforeBuffer:row-state 
            when ValidationHookEventType:Post then
                case phBeforeBuffer:row-state:
                    when row-deleted then
                        DeletePostTransValidate(input dataset-handle hDataset by-reference).
                    when row-modified then
                        WritePostTransValidate(input dataset-handle hDataset by-reference).
                    when row-created then
                        do:
                            CreatePostTransValidate(input dataset-handle hDataset by-reference).
                            WritePostTransValidate(input dataset-handle hDataset by-reference).
                        end.
                end case. // phBeforeBuffer:row-state    
        end case. // pEventType   
            
    end method.
   // 10/16/2023 -  remove after a minth if no problems having CreateSaveSource setting prefer-datset if hidden fields 
    /*    // Workaround for DEFECT OCTA-39312 - Case #:  01028213                                                                                                                           */
    /*    method private logical SaveRowFixBeforebufferBug(phBeforeBuffer as handle):                                                                                                       */
    /*        define variable hAfter as handle no-undo.                                                                                                                                     */
    /*        define variable hBackupBuffer as handle no-undo.                                                                                                                              */
    /*        define variable rRowid as rowid no-undo.                                                                                                                                      */
    /*        define variable lok as logical no-undo.                                                                                                                                       */
    /*        define variable cHiddenFields as character no-undo.                                                                                                                           */
    /*        if phBeforeBuffer:row-state <> row-modified then                                                                                                                              */
    /*             undo, throw new IllegalArgumentError("SaveRowFixBeforebufferBug called with row-state other than modified") .                                                            */
    /*                                                                                                                                                                                      */
    /*        hAfter = phBeforeBuffer:after-buffer.                                                                                                                                         */
    /*        if valid-handle(BackupDataset) then                                                                                                                                           */
    /*        do on error undo, throw:                                                                                                                                                      */
    /*            hBackupBuffer = BackupDataset:get-buffer-handle(hAfter:name).                                                                                                             */
    /*                                                                                                                                                                                      */
    /*            hBackupBuffer:find-unique () no-error.                                                                                                                                    */
    /*            if not hBackupbuffer:avail then                                                                                                                                           */
    /*            do:                                                                                                                                                                       */
    /*                hBackupBuffer:buffer-create.                                                                                                                                          */
    /*            end.                                                                                                                                                                      */
    /*            hBackupbuffer:buffer-copy(phBeforebuffer).                                                                                                                                */
    /*            hBackUpbuffer:table-handle:tracking-changes = true.                                                                                                                       */
    /*            hBackupbuffer:buffer-copy(hAfter).                                                                                                                                        */
    /*            hBackUpbuffer:table-handle:tracking-changes = false.                                                                                                                      */
    /*            catch e as Progress.Lang.Error :                                                                                                                                          */
    /*                undo, throw new ApplicationError(subst("Error in pre processing of &1 in workaround for bad before buffer after save. Please Contact IT.",                            */
    /*                                                        BufferUtil:GetRowName(hAfter)),                                                                                               */
    /*                                                      e).                                                                                                                             */
    /*            end catch.                                                                                                                                                                */
    /*        end.                                                                                                                                                                          */
    /*                                                                                                                                                                                      */
    /*        lok = phBeforeBuffer:save-row-changes(1,"",NoLobs).                                                                                                                           */
    /*                                                                                                                                                                                      */
    /*        if lok                                                                                                                                                                        */
    /*        // data source modified with no-error means save is successful,                                                                                                               */
    /*        // but ALL (not only one) before values are lost and reset from the db because there were differences with db in a field that                                                 */
    /*        // was not saved becuase after and before was the same                                                                                                                        */
    /*        and phBeforeBuffer:data-source-modified                                                                                                                                       */
    /*        and valid-handle(BackupDataset)                                                                                                                                               */
    /*        and hBackupbuffer:before-rowid <> ?                                                                                                                                           */
    /*        and phBeforeBuffer:data-source-modified                                                                                                                                       */
    /*        then                                                                                                                                                                          */
    /*        do on error undo, throw:                                                                                                                                                      */
    /*            hAfter:find-by-rowid(phBeforeBuffer:after-rowid).                                                                                                                         */
    /*                                                                                                                                                                                      */
    /*            rRowid = hAfter:rowid.                                                                                                                                                    */
    /*                                                                                                                                                                                      */
    /*            do on error undo, throw:                                                                                                                                                  */
    /*                phBeforeBuffer:reject-row-changes.                                                                                                                                    */
    /*                catch e as Progress.Lang.Error :                                                                                                                                      */
    /*                    cHiddenFields = BufferUtil:GetHiddenFields(hAfter).                                                                                                               */
    /*                    if cHiddenFields > "" then                                                                                                                                        */
    /*                       BufferUtil:GetFieldLabels(hAfter, cHiddenFields) .                                                                                                             */
    /*                                                                                                                                                                                      */
    /*                    undo, throw new ApplicationError(                                                                                                                                 */
    /*                       subst("Repply of &1 changes during post processing of workaround for bad before buffers &2 caused a conflict with other submitted records. Please Contact IT.",*/
    /*                                BufferUtil:GetRowName(hAfter),                                                                                                                        */
    /*                                if cHiddenFields > "" then subst(" likely due to hidden fields: &1, ",cHiddenFields) else ""                                                          */
    /*                            ),                                                                                                                                                        */
    /*                       e).                                                                                                                                                            */
    /*                end catch.                                                                                                                                                            */
    /*            end.                                                                                                                                                                      */
    /*            hAfter:find-by-rowid(rRowid).                                                                                                                                             */
    /*            hBackupbuffer:before-buffer:find-by-rowid (hBackupbuffer:before-rowid).                                                                                                   */
    /*            hAfter:buffer-copy(hBackupbuffer:before-buffer).                                                                                                                          */
    /*            hAfter:table-handle:tracking-changes = true.                                                                                                                              */
    /*            hAfter:buffer-copy(hBackupbuffer).                                                                                                                                        */
    /*            hAfter:table-handle:tracking-changes = false.                                                                                                                             */
    /*            phBeforebuffer:find-by-rowid(hAfter:before-rowid).                                                                                                                        */
    /*            hBackupbuffer:before-buffer:reject-row-changes.                                                                                                                           */
    /*            catch e1 as ApplicationError :                                                                                                                                            */
    /*                undo, throw e1.                                                                                                                                                       */
    /*            end.                                                                                                                                                                      */
    /*            catch e2 as Progress.Lang.Error :                                                                                                                                         */
    /*                undo, throw new ApplicationError(subst("Error in post processing of &1 in workaround for bad before buffer after save. Please Contact IT.",                           */
    /*                                                       BufferUtil:GetRowName(hAfter)),                                                                                                */
    /*                                                 e2).                                                                                                                                 */
    /*            end catch.                                                                                                                                                                */
    /*                                                                                                                                                                                      */
    /*        end.                                                                                                                                                                          */
    /*                                                                                                                                                                                      */
    /*        return lok.                                                                                                                                                                   */
    /*    end method.                                                                                                                                                                       */
    
    method public logical SaveRow(phBeforeBuffer as handle):
        define variable lOk as logical no-undo.
        define variable cAllChanges as character no-undo.
        define variable cChanges as character no-undo.
        define variable oSaveUtil as SaveUtil no-undo. 
       
        // save-row-changes for delete will optimistic lock check every field
        // FindAndCompareSource ignores the serialize-hidden fields
        if phBeforeBuffer:row-state = row-deleted then
        do: 
            // NoLobs is honored in the compare and ProxyManager:CheckProxyConflicts will be called  
            FindAndCompareSource(phBeforeBuffer).
            lok = BufferHandle:buffer-delete().
            return lok.      
        end.
        else do:
            if phBeforebuffer:row-state = row-modified then
            do: 
                if valid-object(FieldSaveManager) then 
                do:
                    cChanges = FieldSaveManager:CheckRow(this-object,phBeforeBuffer).
                    if cChanges > "" then 
                        cAllChanges = StringUtil:AppendEntry(cAllChanges, cChanges).
                end.    
                if valid-object(ProxyManager) then
                do:
                    cChanges = ProxyManager:CheckRow(this-object,phBeforeBuffer).
                    if cChanges > "" then 
                        cAllChanges = StringUtil:AppendEntry(cAllChanges, cChanges).
                end.
                
                if valid-object(ProxyManager) and ProxyManager:HasKey then
                do:
                    oSaveUtil = new SaveUtil().
                    cChanges = oSaveUtil:CheckRow(this-object,phBeforeBuffer).
                    if cChanges > "" then 
                        cAllChanges = StringUtil:AppendEntry(cAllChanges, cChanges).
                    
                    if cAllChanges > "" then 
                        undo, throw new ConflictError(phBeforeBuffer,BufferUtil:GetFieldLabels(Databuffer,cAllChanges)).
                    
                    lok  = oSaveUtil:SaveRow(this-object,phBeforeBuffer).
                    
                end.     
                else do:                                                         
                    if cAllChanges > "" then 
                        undo, throw new ConflictError(phBeforeBuffer,BufferUtil:GetFieldLabels(Databuffer,cAllChanges)).
                    
                    // CreateSaveSource sets prefer-dataset true and merge-by-field false  if there are any 
                    // hidden fields  to workaround
                    // DEFECT OCTA-39132 
                    // - Case #:  01028213 - bug work around -
                    // SAVE-ROW-CHANGES causes before table buffer to get updated with the values from the AFTER-BUFFER
                    // changed to documentation bug...
                    // so we need a workaround, and call FindAndCompareSource to check for changes since these settings
                    // supresses optimistic lock from save-row-changes    
                    if DataSourceHandle:prefer-dataset and DataSourceHandle:merge-by-field = false then
                    do: 
                        FindAndCompareSource(phBeforeBuffer).
                        // save-row-changes will update all values, so hiddenfields witrh no changes
                        // will get the default value
                        // - so if the before and after is the same set after from the DB 
                        //   so that it does not overwrite                        
                        ApplyHiddenFieldsValuesFromDB(phBeforebuffer,BufferHandle).
                    end.    
                    lok = phBeforeBuffer:save-row-changes(1,"",NoLobs). 
                end. 
            end.
            else do: // create
                if valid-object(SourceFieldManager) then
                do: 
                    // checkrow in sourcefieldmanager is hijacked for prepare for create
                    // (the manager is only used for crease and do need optitic lock check) 
                    // This is an otptional step typcially used if a constant proxy 
                    //  need to access a buffer to get current last value
                    SourceFieldManager:CheckRow(this-object,phBeforeBuffer).
                end.
                
                // use SaveUtil if key is managed by manager since 
                // The source is not avail after save-row-changes and the managers wont be able to find the new record if key is not assigned
                // so the field mangers are thus expecting a creatred record to be available 
                if (valid-object(SourceFieldManager) and SourceFieldManager:HasKey) 
                    or (valid-object(ProxyManager) and (ProxyManager:HasKey or ProxyManager:HasMandatoryInitNull)) 
                    or (valid-object(FieldSaveManager) and (FieldSaveManager:HasKey or FieldSaveManager:HasMandatoryInitNull)) then
                do:
                    oSaveUtil = new SaveUtil().
                    lok = oSaveUtil:SaveRow(this-object, phBeforeBuffer).
                end.
                else do:      
                   
                    lok = phBeforeBuffer:save-row-changes(1,"",NoLobs).
                    // annoying behavior - the record is lost during save-row-changes  
                    phBeforebuffer:after-buffer:find-by-rowid (phBeforebuffer:after-rowid).  
                    FindSource(exclusive-lock). 
                end.   
                if lok and valid-object(SourceFieldManager) then 
                do:
                    SourceFieldManager:SaveRow(this-object, phBeforeBuffer).
                end.    
            end.    
            if valid-object(FieldSaveManager) then
            do:
                lok = FieldSaveManager:SaveRow(this-object,phBeforeBuffer).
            end.
            if valid-object(ProxyManager)  then
                lok = ProxyManager:SaveRow(this-object,phBeforeBuffer).
            return lok.
        end.
        catch e1 as ConflictError:
            undo, throw e1.
        end.
        catch e2 as class Error:
            SaveRowErrorHandler(e2, phBeforeBuffer, UpdateSource,cAllChanges).
        end.
         
    end method.
    
    // hidden fields are not attached and prefer-dataset and DataSourceHandle:merge-by-field is set to true 
    // to ensure before values are not overridden in save-row-changes 
    // see comments in SaveRow  about-DataSourceHandle:prefer-dataset
    // This method ensures that hdiden fields that actually have been changed in the BE are saved  
    method private void ApplyHiddenFieldsValuesFromDB(phBeforeBuffer as handle, phDbSource as handle):
        define variable cHiddenFields as character no-undo.
        define variable i             as integer no-undo.
        define variable hField        as handle no-undo.
        define variable hBeforeField  as handle no-undo.
        define variable cDbField      as character no-undo.
        define variable hDbField      as handle no-undo.
        define variable hBuffer       as handle no-undo.
        define variable cField        as character no-undo.
        define variable iArrayPos     as integer no-undo.
        define variable iExtent       as integer no-undo.
        // annoying behavior - the record is lost during save-row-changes  
        hBuffer = phBeforebuffer:after-buffer. 
        hBuffer:find-by-rowid (phBeforebuffer:after-rowid).  
        cHiddenFields = BufferUtil:GetHiddenFields(hBuffer).
                
        do i = 1 to num-entries(cHiddenFields):
            cField = entry(i,CHiddenFields). 
            // skip the seqence - not really a problem since there will be no db field.. 
            if cField <> BufferUtil:SeqFieldName then
            do: 
                hField       = hBuffer:buffer-field(entry(i,CHiddenFields)).
                hBeforeField = phBeforebuffer:buffer-field(entry(i,CHiddenFields)).
                if hField:buffer-value = hBeforeField:buffer-value then
                do:  
                    cDbField = StringUtil:GetMappedName(subst("&1.&2",hBuffer:name,hField:name),hBuffer:data-source-complete-map).
                    if cDbField > "" and cDbField begins BufferHandle:name + "." then
                    do:
                        iArrayPos = index(cDbField,"[").  
                        if iArrayPos > 0 then
                        do:
                            iExtent = integer(right-trim(substring(cDbField,iArrayPos + 1),"]")).
                            cDbField = substring(cDbField,1,iArrayPos - 1).
                        end.
                        hDbfield = BufferHandle:buffer-field(entry(2,cDbField,".")) no-error.   
                        if valid-handle(hDBField) then 
                            hField:buffer-value = hDbField:buffer-value(IarrayPos).                             
                    end.         
                end.
            end.  
        end.    
          
    end method.    
    
    method public logical CustomOperation(pcObject as Object, phDataset as handle).
    end method.
    
    // hook to define table actions to be called from ExecuteActionasTransaction
    method public character GetTransactionTableActions(): 
        return "".  
    end method. 
    
    method public Method GetActionMethod(pAction as IAction,input phDataset as handle,output pParameterList as ParameterList).
        
        define variable oIterator    as IIterator no-undo.
        define variable oMethod      as Method    no-undo.
        define variable oChildAction as IAction   no-undo.
        
        if pAction:TargetTableName = DataBuffer:table-handle:name then
        do:
            pParameterList = pAction:ParameterList.
        
            if not valid-object(pParameterList) then
                pParameterList = new ParameterList(1).
        
            pParameterList:SetParameter(pParameterList:NumParameters, "dataset-handle", "input by-reference", phDataset).
            oMethod = GetClass():GetMethod(pAction:Name,Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance, pParameterList).
        end.
        /*        else if pAction:HasChild then                                                                            */
        /*        do:                                                                                                      */
        /*            oIterator = pAction:Children:Iterator().                                                             */
        /*            do while oIterator:HasNext():                                                                        */
        /*                oChildAction = cast(oIterator:Next(),IAction).                                                   */
        /*                oMethod = GetActionMethod(oChildAction,phDataset, output pParameterList).                        */
        /*                // return if valid (found in the child tree) or if we have found the right table return also if ?*/
        /*                if valid-object(oMethod) or oChildAction:TargetTableName = DataBuffer:table-handle:name then     */
        /*                    return oMethod.                                                                              */
        /*            end.                                                                                                 */
        /*        end.                                                                                                     */
        return oMethod.
    end method.
    
    method protected character GetRowState(input iRowState as integer):
        define variable cRowState as character no-undo.
        case iRowState:
            when row-deleted then
                cRowState = "Delete".
            when row-created then
                cRowState = "Create".
            when row-modified then
                cRowState = "Update".
            otherwise
            cRowState = "Unmodified".
        end case.
         
        return cRowState .
    end method.   
    
    method protected character GetRowErrorMessage(pcMessage as char,piState as integer):
        return pcMessage.
    end.    
    
    
    /* convert expression  */
    /* (part of IQueryMap interface  
      final - override  ColumnExpression(QueryPredicate)*/
    method public final IQueryEntry ColumnExpression(pPredicate as IQueryPredicate):
        define variable cValues as character extent no-undo.
        
        if type-of(pPredicate,QueryPredicate) or type-of(pPredicate,CompareQueryPredicate) then 
        do:
            return ColumnExpression(cast(pPredicate,QueryPredicate)).
        end. 
        else do:
            if valid-object(pPredicate:Values) then
                return ColumnExpression(new QueryPredicate(pPredicate:Join,pPredicate:FieldName,pPredicate:Operator,Array:ToStringArray(pPredicate:Values))).
            else
                return ColumnExpression(new QueryPredicate(pPredicate:Join,pPredicate:FieldName,pPredicate:Operator,string(pPredicate:Value))).
        end.     
    end method.   
  
    method public IQueryEntry ColumnExpression(pPredicate as QueryPredicate):
        define variable oProxyPredicate as IQueryEntry no-undo. 
        
        if valid-object(ProxyManager) then 
        do:
            oProxyPredicate = ProxyManager:ColumnExpression(pPredicate).
            if valid-object(oProxyPredicate) then 
                return oProxyPredicate. 
        end.    
        
        return pPredicate. 
    end method.   
    
    /** convert sort expression column for QueryString 
       (second parse - column already transformed by ColumnSource(pccolumn)  
       (part of IQueryMap interface used by QueryString to map query) */
    method public IQuerySortEntry extent ColumnSort(pSort as IQuerySortEntry):
        define variable oSort      as IQuerySortEntry extent 1 no-undo.
        define variable oProxySort as IQuerySortEntry extent no-undo.
        
        if valid-object(ProxyManager) then 
        do: 
            oProxySort = ProxyManager:ColumnSort(pSort).
            if extent(oProxySort) > 0 then 
                return oProxySort.  
        end.    
        
        oSort[1] = pSort.
        return oSort.     
    end method.   
      
    /* return source columns for query mapping 
    (part of IQueryMap interface used by QueryString to map query */
    method public character ColumnSource (pcColumn as char):
        define variable iLookup     as integer     no-undo.
        define variable cTable      as character   no-undo.
        define variable hBuffer     as handle      no-undo.
        define variable hDataset    as handle      no-undo.
        define variable cDbColumn   as character   no-undo.
        define variable joinedsrc   as IDataSource no-undo.
        define variable cJoinCol    as character   no-undo.
        define variable hFld        as handle      no-undo.
        define variable i           as integer     no-undo.
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        define variable cProxyColumn as character no-undo.
        define variable cColumnName as character no-undo.
        define variable cMappedName as character no-undo.
        
        /*  inner join reference to other buffers can be passed from client,
            so we use the buffer's dataset to get those datasources' 
            fieldmap. This assume that any buffer that has been attached to a 
            datasource is referencing the same physical storage, (keep in mind that all connected 
            progress databases are the same physical storage in this context), so all dataset mapping 
            is valid for this datasource. 
            Note that this secondary inner join is passed from the client as an option to reduce the 
            number of records to fill for THIS table. 
            The actual fill of the joined table is handled by another datasource.  */
        if lookup(pcColumn,ProxyFieldNames) > 0 and valid-object(ProxyManager) then 
        do: 
            cProxyColumn = ProxyManager:ColumnSource(pcColumn).
            if cProxyColumn > "" then 
                return cProxyColumn.  
        end.    
        
        if num-entries(pcColumn,".") = 1 then 
        do:        
            // not attached - use FieldMap                
            if not valid-handle(DataBuffer:data-source) then
            do:
                iLookup = lookup(pcColumn,FieldMap).   
                if iLookup > 0 then
                    return entry(iLookup + 1,FieldMap).
                else do i = 1 to extent(BufferHandles):
                    hfld = BufferHandles[i]:buffer-field(pcColumn) no-error.
                    if valid-handle(hFld) then
                        return subst("&1.&2",BufferHandles[i]:name,pcColumn).
                end.  
                return pcColumn.   
            end.             
            // TODO this is a bit messy - need clenaup, but lots of testing needed  
            cColumnName = pcColumn.
            pcColumn = DataBuffer:name + "." + pcColumn.
            hBuffer  = DataBuffer.
        end.
        else do: 
            cTable = entry(1,pcColumn,".").
            cColumnName = entry(2,pcColumn,".").
            hDataset = DataBuffer:dataset.
            if DataBuffer:name = cTable then
                hBuffer = DataBuffer.
            else do:
                /* keep reference to parent relation as is  */
                if valid-handle(ParentRelation) and cTable = ParentRelation:parent-buffer:name then
                    return pcColumn.
                   
                hBuffer  = hDataSet:get-buffer-handle(cTable) no-error.     
            end.
        end.
        if valid-handle(hBuffer) then
        do:
            
            iLookup = lookup(pcColumn,hBuffer:data-source-complete-map).
            if iLookup > 0 then
            do:
                cDbColumn =  entry(iLookup + 1,hBuffer:data-source-complete-map).
                if hBuffer <> DataBuffer and lookup(entry(1,cDbColumn,"."),BufferNames) = 0 then
                    return pcColumn.
                return cDbColumn.
            end.
            else do:
                // fields in field  save manager is excluded from attahc and thus not in data-source-complete-map 
                if valid-object(FieldSaveManager) then 
                do:
                    if lookup(cColumnName,FieldSaveManager:FieldNames) > 0 then 
                    do: 
                        cMappedname = StringUtil:GetMappedName(cColumnName,FieldMap). 
                        if cMappedname > "" then 
                            return cMappedName.    
                        else 
                            return subst("&1.&2",BufferName,cColumnName).   
                    end.
                end.     
                return pcColumn.
            end.    
        end.
        catch e as Progress.Lang.Error :         
            undo, throw new IllegalArgumentError(subst("Error when trying to resolve '&1' against data source",pcColumn),e).
        end catch.  
    end method.                   
   
    // create a temp buffer for a qualified field name for calls that uses datasource for schema info  
    method protected character GetUnpreparedFieldMap(pcDbColumn as character):
        define variable hBuffer as handle no-undo.
        if num-entries(pcDBColumn,".") = 2 then
        do: 
            if not valid-handle(Databuffer) then 
            do: 
                if extent(BufferHandles) = ? then
                do:
                    create buffer hbuffer for table entry(1,pcDBcolumn,".").
                    extent(BufferHandles) = 1.
                    BufferHandles[1] = hBuffer.
                end.   
                return getFieldMap().
            end.    
            else 
                undo, throw new IllegalOperationError("Call to GetUnpreparedFieldMap when prepared. Please Contact IT.").
        end.
        else 
            undo, throw new IllegalArgumentError("Unqualfied Db Column parameter. Please Contact IT.").
        
        finally:
            if valid-handle(hBuffer) then 
            do:
                delete object hBuffer.
                extent(BufferHandles) = ?.                  
            end.
        end.
    end.
    
    /*
    Get the TT column mapped to the database field    
    Will return data also if DataSOurcer is no prepared, but this 
    is not 100% reliable as the field may not exist in the TT when prepared 
    */
    method public character SourceColumn (pcDbColumn as char):
        define variable iLookup    as integer    no-undo.
        define variable cFieldmap  as character no-undo.
       
        if num-entries(pcDbColumn,".") <> 2 then 
            undo, throw new IllegalArgumentError(subst("Database field name '&1' must be qualified with table name",pcDbColumn)).      
        
        // support schema info when bnot prepared used by AlertArguments   
        if not valid-handle(Databuffer) then 
        do: 
            cFieldmap = GetUnpreparedFieldMap(pcDbColumn).
            iLookup = lookup(pcDbColumn,cFieldmap).
            if iLookup > 0 then
                return entry(iLookup - 1,cFieldmap).
            else 
                return entry(2,pcDBcolumn,".").        
        end.    
        else do:
            iLookup = lookup(pcDbColumn,DataBuffer:data-source-complete-map).
            if iLookup > 0 then
                return entry(iLookup - 1,DataBuffer:data-source-complete-map).
              
            return "".   
        end. 
    end method.                   
    
    // not in interface - currently used by AlertSDA
    method public handle ColumnSource (phbuffer as handle, pcDbColumn as char, piExtent as integer, output piTTArray as integer):
        define variable cMap           as character no-undo.
        define variable iLookup        as integer   no-undo.
        define variable cMappedName    as character no-undo.
        define variable hBufferField   as handle    no-undo.
        define variable cColumn        as character no-undo.
        define variable iDbArray       as integer   no-undo.
        define variable cDBMapColumn   as character no-undo.
        define variable iArrayPos      as integer   no-undo.
       
        if num-entries(pcDbColumn,".") <> 2 then 
            undo, throw new IllegalArgumentError(subst("Database field name '&1' must be qualified with table name",pcDbColumn)).      
        
        if phBuffer:serialize-name <> EntityName then 
            undo, throw new IllegalArgumentError(subst("Cannot get database schema information for buffer '&1' from DataSource '&2'",phBuffer:name,EntityName)).      
        
        if entry(1,pcDbColumn,".") <> BufferName then 
            undo, throw new IllegalArgumentError(subst("Cannot get database schema information for table '&1' from DataSource '&2'",entry(1,pcDbColumn,"."),EntityName)).      
        
        if not valid-handle(Databuffer) then
            cMap = GetUnpreparedFieldMap(pcDbColumn).
        else     
            cMap = GetFieldMap().
        
        if cMap > "" then
        do: 
            cDBMapColumn = if piExtent = 0 then pcDbColumn else subst("&1[&2]",pcDbColumn,piExtent).
          
            iLookup = lookup(cDBMapColumn,cMap).
            if iLookup > 0 then
                cColumn = entry(iLookup - 1,cMap).
               
        end.
        if iLookup = 0 then
            cColumn = entry(2,pcDbColumn,".").
        iArrayPos = index(cColumn,"[").  
        if iArrayPos > 0 then
        do:
            if iArrayPos > 0 then
            do:
                piTTArray = integer(right-trim(substring(cColumn,iArrayPos + 1),"]")).
                cColumn = substring(cColumn,1,iArrayPos - 1).
            end.   
        end.
        
        hBufferField = phbuffer:buffer-field(cColumn) no-error.
        if not valid-handle(hBufferField) then
            undo, throw new IllegalArgumentError(subst("Database field '&1' is not mapped to DataSource &2",pcDbColumn,EntityName)).      
        return HBufferField.
           
    end method.     
    
    method protected void FindSources(pLock as integer):
        define variable hBeforebuffer as handle no-undo.
        define variable i as integer no-undo.
        hBeforebuffer = BufferUtil:AssertAndGetBeforebuffer(DataBuffer).
        do i = 1 to extent(BufferHandles):
            FindSource(hBeforebuffer,BufferHandles[i],pLock).
        end.               
    end method.
    
    // void - throws error if not found
    method public void FindSource(plock as integer):
        FindSource(DataBuffer,BufferHandle,pLock). 
    end.    
    
    method public void FindSource(phBuffer as handle, pLock as integer):
         FindSource(phBuffer,BufferHandle,pLock). 
    end method.
     
    // void - throws error if not found
    method public void FindSource(phbuffer as handle,pDbBuffer as handle,plock as integer):
        define variable cWhere as character no-undo.
        define variable oError as AppError     no-undo.
        cWhere = SourceWhere(phbuffer,pDbBuffer).
        
        pDbBuffer:find-unique (cWhere,plock). 
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 138 or e:GetMessageNum(1) = 3166 then
            do: 
                oError = new TableNotFoundError(phbuffer,GetFindCurrentRowRequest(phBuffer)).
                oError:AddMessage(subst("Database query 'find &1 &2' throws error:~n&3",BufferName,cWhere,e:GetMessage(1)),?).  
                undo, throw oError.
            end.
            else 
                undo, throw e.
        end catch.
    end.    
    
    method protected character SourceWhere(phbuffer as handle, phDbBuffer as handle):
        define variable cDbKeys     as character no-undo.
        define variable cDbKey      as character no-undo.
        define variable cKey        as character no-undo.
        define variable i           as integer no-undo.
        define variable iSource     as integer no-undo.  
        define variable cWhere      as character no-undo.
        define variable cAnd        as character no-undo.
        define variable hFld        as handle no-undo.
        define variable cFieldWhere as character no-undo.
        define variable cValue      as character no-undo.
        define variable lUseBefore  as logical no-undo.
        define variable cErrorTmpl as character 
            init  "Buffer '&1' is not a before or after buffer for the Data Buffer table '&2'"
            no-undo.
        if not phBuffer:avail then
            undo, throw new IllegalStateError(subst("Buffer '&1' is not available",phBuffer:name)).
         
        if valid-handle(phbuffer:after-buffer) then
        do:  
            if phBuffer:after-buffer:table-handle <> DataBuffer:table-handle then
            do:
                undo, throw new IllegalOperationError(subst(cErrorTmpl,phBuffer:name,DataBuffer:table-handle:name)).
            end.
            // use to decide how to get exprtression from proxy
            lUseBefore = phBuffer:row-state <> row-created.
        end.
        else if phBuffer:table-handle <> DataBuffer:table-handle then
            undo, throw new IllegalOperationError(subst(cErrorTmpl,phBuffer:name,DataBuffer:table-handle:name)).
          
        if valid-handle(phbuffer:after-buffer) and phBuffer:row-state = row-created then  
            phBuffer = phbuffer:after-buffer.
          
        cDbKeys = GetDataSourceKeys(phDbBuffer).
         
        cAnd = " where".
        if cDbKeys = 'rowid' then 
        do:
            undo, throw new IllegalStateError(subst("Cannot find a unique record for class &1 because the database table key is rowid. A unique key consisting of field name(s) must be defined. This can be done in a GetDataSourceKeys override in the class.", this-object:GetClass():TypeName)).
        end.    
         
        do i = 1 to num-entries(cDbKeys):
            assign
                cDbKey = subst("&1.&2",phDbBuffer:name,entry(i,cDbKeys))
                cKey   = SourceColumn(cDbKey)
                .
             // if the key is not mapped check if it is managed as a field object    
            if cKey = "" then 
            do: 
                if valid-object(SourceFieldManager) then
                do: 
                     // will throw illegalState if a IHiddenSourceKey is reached here
                     // The error eill explain that a hidden source key must not be in GetfieldMap and not returned from GetDataSourceKeys 
                    cFieldWhere  = SourceFieldManager:WhereExpression(phDbBuffer, entry(i,cDbKeys)).
                end.
                if cFieldWhere = "" 
                    and valid-object(ProxyManager) then 
                do:
                    cFieldWhere = if lUseBefore 
                                  then ProxyManager:BeforeWhereExpression(phDbBuffer, entry(i,cDbKeys))
                                  else ProxyManager:WhereExpression(phDbBuffer, entry(i,cDbKeys)).   
                end.
                if cFieldWhere = "" then
                    undo, throw new IllegalStateError(subst("Database field &1 is not mapped and not defined as constant or proxy",cDbKey)).     
            end.
            else do:
                assign
                    hFld   = phBuffer:buffer-field(entry(2,cKey,"."))
                    cValue = SourceWhereFieldValue(hFld).
                     
                if hFld:data-type = "character" or hFld:data-type = "longchar" or hFld:data-type begins "date" then 
                    cValue = quoter(cValue).
                cFieldWhere = subst("&1 = &2",cDbKey,cValue).
            end.  
                   
            assign   
                cWhere = cWhere + subst("&1 &2",cAnd,cFieldWhere)
                cAnd   = " and"
                .        
        end.
         
        return cWhere.
    end.    
    
    // allow override for data type conversion and dynamic resolution 
    method protected character SourceWhereFieldValue(phField as handle) :
        return phField:buffer-value.
    end method.    
    
    method public void FindAndCompareSource():
        if DataBuffer:avail = false and DataBuffer:before-buffer:avail and DataBuffer:before-buffer:row-state = row-deleted then
            FindAndCompareSource(DataBuffer:before-buffer).
        else     
            FindAndCompareSource(DataBuffer).
    end method.
    
    // default find for unmodified in transaction  find and compare exclusive  
    method public void FindSourceForUnmodifiedRow().
        FindSource(exclusive-lock).
    end method. 
    
    method public void FindAndCompareSource(phBuffer as handle):
        FindAndCompareSource(phbuffer,"").
    end method.
    
    method public void FindAndCompareSource(phBuffer as handle, pcfields as character):
        define variable cChanges as character no-undo.
        define variable lchanged as logical no-undo.
        define variable i as integer no-undo.
        define variable hFld as handle no-undo.
        define variable cFld as character no-undo.
        define variable cPublicChanges as character no-undo.
        lchanged = FindAndCompareSource(phbuffer, pcfields, output cChanges).
        if lchanged then 
        do:
            do on error undo, throw:
                cPublicChanges = BufferUtil:GetFieldLabels(Databuffer,cChanges).
                // if there are spaces in the changes the ConflcitError will use as-is 
                // so  we nee to beatify before we pass
                cPublicChanges = StringUtil:GetMessageFromEntries(cPublicChanges, "and", true). // true add quotes 
                // needs more testing so for now jusat use the internal names if there is an error
                catch e as IllegalArgumentError:
                    cPublicChanges = pcFields.     
                end catch.
            end.        
            
            undo, throw new ConflictError(phbuffer,cPublicChanges).
        end.    
    end method.
    
    method protected logical FindAndCompareSource(phbuffer as handle, output pChanges as character):
        return FindAndCompareSource(phbuffer, "", output pChanges).
    end method.
    
    method protected logical FindAndCompareSource(phBuffer as handle, input pcfields as character, output pcChanges as character):
        
        if not phBuffer:avail then
            undo, throw new IllegalStateError(subst("Buffer '&1' is not available",phBuffer:name)).
            
        if phbuffer = DataBuffer:before-buffer then
        do:
            if phBuffer:row-state = row-created then
            do: 
                phBuffer = Databuffer.
                if phBuffer:avail = false or phBuffer:rowid <> DataBuffer:before-buffer:after-rowid then
                    phBuffer:find-by-rowid (DataBuffer:before-buffer:after-rowid).  
            end.    
        end.
        else if phBuffer = DataBuffer then 
        do: 
            if phBuffer:row-state = row-modified then
            do: 
                phBuffer = DataBuffer:before-buffer.
                if phBuffer:avail = false or phBuffer:rowid <> DataBuffer:before-rowid then
                    phBuffer:find-by-rowid (DataBuffer:before-rowid).
            end.    
        end.
        else 
            undo, throw new IllegalStateError(subst("Buffer '&1' is not a valid buffer reference for DataSource '&2'",phBuffer:name,EntityName)).
        FindSource(phbuffer, BufferHandle, exclusive-lock).
        pcChanges = CompareBufferWithDb(phbuffer,pcfields).
        
        return pcChanges > "".  
    end method.
    
    method protected void AssertRowIsCurrent(pRowState as integer):
        AssertRowIsCurrent(pRowState,"").   
    end method.    
    
    method protected void AssertRowIsCurrent(pRowState as integer, pcFields as character):
        define variable hBuffer  as handle no-undo.
        define variable cChanges as character no-undo.
        if pRowState = row-modified 
            or pRowState = row-deleted then
            hBuffer = DataBuffer:before-buffer.
        else 
            hbuffer = DataBuffer.    
        cChanges = CompareBufferWithDb(hbuffer,pcfields).
        if cChanges > "" then 
            undo, throw new ConflictError(hbuffer).      
    end method.    
    
    method protected void ThrowConflictError(phBeforebuffer as handle):
        undo, throw new ConflictError(phBeforeBuffer). 
    end method.    
    
    method protected void ThrowConflictError(phBeforebuffer as handle,pcchanges as character):
        undo, throw new ConflictError(phBeforeBuffer,pcChanges). 
    end method. 
    
    // for subclaseesa that does not managen changews this way 
    method protected void SaveRowErrorHandler  (pe as Error, phBeforeBuffer as handle, piSource as integer):
        SaveRowErrorHandler(pe,phBeforeBuffer,piSource,"").
    end method. 
    
    method protected void SaveRowErrorHandler  (pe as Error, phBeforeBuffer as handle, piSource as integer, pcChanges as character):
        define variable cMsg              as character no-undo.
        define variable cField            as character no-undo.
        define variable hField            as handle no-undo.
        define variable cKeyExpression    as character no-undo.
        define variable hDataFields       as handle extent no-undo.
        define variable hSourceFields     as handle extent no-undo.
        define variable cChanges          as character no-undo.
        
        // don't  wrap illlegalstate  
        if type-of(pe,IllegalStateError) then 
            undo, throw pe. 
        cMsg = pe:GetMessage(1).
        
        if piSource > 0 then 
        do: 
            //cMsg = right-trim(replace(cMsg,subst("(&1)",pe:GetMessageNum(1)),"")).
            case pe:GetMessageNum(1):
                // lead attributes due to invalid handle and bad data-source-complete ref errors then throw as-is to get full staclk 
                when 10068 or when 12780 then 
                    undo, throw pe.
                
                when 11894  then // KEYS phrase for data source buffer <db buffer> contains ROWID and no filed in <before image table> is mapped to the rowid durng attach source                          
                    do: 
                        cMsg = subst("Cannot save &1 changes ",BufferUtil:GetRowName(DataBuffer))
                            + subst("because the database table key is rowid. A unique key consisting of field name(s) must be defined. This can be done in a GetDataSourceKeys override in the &1 DataSource.",
                            this-object:GetClass():TypeName).
                    end.    
                when 11913  then // SAVE-ROW-CHANGES found <name> record with conflicting change by another user. (11913) 
                    do on error undo, throw: 
                        Databuffer:find-by-rowid (phBeforeBuffer:after-rowid).
                        FindSources(exclusive-lock).
                    
                        cChanges = CompareBufferWithDb(phBeforeBuffer, "", output hDataFields, output hSourceFields).
                    
                        if cChanges > "" then 
                            StringUtil:AppendEntry(pcChanges, cChanges). 
                    
                        if pcChanges > "" then     
                            ThrowConflictError(phBeforebuffer,BufferUtil:GetFieldLabels(Databuffer,pcChanges)).
                        else
                            ThrowConflictError(phBeforebuffer).  
                        catch e1 as ConflictError :
                            undo, throw e1.
                        end.
                        catch e2 as Progress.Lang.Error :
                            BufferError(phBeforeBuffer, GetRowErrorMessage(e2:GetMessage(1),phBeforeBuffer:row-state)).
                        end catch.     
                    end.    
                when 132  then 
                    do: //** <file-name> already exists with <field/value...>. (132) 
                        cMsg = GetPublicAlreadyExistsMessage(cmsg, phBeforebuffer, piSource).
                    end.
                when 138 // <file-name> record not on file
                or 
                when 11912 /* Unable to find record for db buffer <name> during SAVE-ROW-CHANGES. */ then 
                    do: //** <file-name> record not on file. (132)
                    // use the QueryUtil to get the QueryPredicate:ToString(buffer,useserializename)
                        cKeyExpression = cast(QueryUtil:GetCurrentRowKeyEntry(phBeforeBuffer),Pmfo.Core.BusinessLogic.IQueryEntry):ToString(phBeforeBuffer,true).
                        cMsg = subst("&1 where &2 does not exist",BufferUtil:GetRowName(phBeforeBuffer,true),cKeyExpression).
                    end.
                when 275 or when 110 then 
                    do: // ** <Field-name> is mandatory, but has a value of ?. (275)
                     // ** <file-name>.<field-name> is mandatory, but has unknown (?) value. (110). 
                        cField = entry(2,cMsg," ").
                        if num-entries(cField,".") > 1 then 
                            cField = entry(2,CField,".").
                    
                        hfield = phBeforebuffer:buffer-field(cField) no-error.
                        if valid-handle(hField) then
                        do:
                            entry(2,cMsg," ") = BufferUtil:GetFieldLabel(hField).
                        end.     
                    end.
                otherwise do:
                    // DataAccess jcjust return "ERROR" as string if dataset error, so make sure
                    // the error is added to the buffer to reach the client
                    // (TODO? num-messages? probably not important as the secondary ABL messages are typically not helpful   
                    if phBeforeBuffer:dataset:error then
                        cMsg = pe:GetMessage(1).
                    else     
                        undo, throw pe.
                end.       
            end.
        end.
        BufferError(phBeforeBuffer, GetRowErrorMessage(cMsg,phBeforeBuffer:row-state)).
    end method.
 
    /* rerhink this serialize-name is not unique on server 
    method protected IDataSource GetDataSourceForDataset(pcserializename as character, phDataset as handle, output pTablehandle as handle):
        define variable hBuffer as handle no-undo.
        hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset, pcserializename).
        if not valid-handle(hBuffer) then
            undo, throw new IllegalStateError(subst("The request is missing data for &1",BufferUtil:SingularizeSerializeName(pcSerializeName,true))).
        pTableHandle = hbuffer:table-handle.
        return GetDataSourceMap(phDataset):Get(hBuffer).
    end method.    
    */
    method protected void DefineQuestion (pId as integer):
        define variable oKey as String no-undo.
        oKey = new OpenEdge.Core.String(string(pid)).
        if Questions:ContainsKey(oKey) then
            undo, throw new IllegalArgumentError(subst("Question is already defined with Id &1",pid)).
        Questions:Put(oKey,new Question(pid)).
    end method.
    
    method protected void DefineQuestion (pId as integer, pcMessage as char):
        define variable oKey as String no-undo.
        oKey = new OpenEdge.Core.String(string(pid)).
        if Questions:ContainsKey(oKey) then
            undo, throw new IllegalArgumentError(subst("Question is already defined with Id &1",pid)).
        Questions:Put(oKey,new Question(pid,pcMessage)).
    end method.
    
    method protected IQuestion GetQuestion(pId as integer):
        define variable oKey as String no-undo.
        define variable oObject as Object no-undo.
        define variable oQuestion as IQuestion no-undo.
        // bug in map? -  did not work with IInt64holder
        oKey = new OpenEdge.Core.String(string(pid)).
        oObject = Questions:Get(oKey).
        if Questions:ContainsKey(oKey) then
            oQuestion = cast(Questions:Get(oKey),IQuestion).
        else 
            undo, throw new IllegalOperationError(subst("Question with id &1 is not defined",pId)).
        return oQuestion.
    end method.    
    
    method protected logical IsQuestionConfirmed(pId as integer):
        return ConfirmedQuestions > "" and lookup(string(pid),ConfirmedQuestions) > 0.  
    end.
     
    method protected logical AskQuestion(pId as integer):
         
        define variable oQuestion as IQuestion no-undo.
        if IsQuestionConfirmed(Pid) then 
            return true.
        // if there already are regular errors then throw them first 
            
        if DataBuffer:dataset:error   then      
            undo, throw new TransactionError(). 
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Question &1 has no predefined message. Use AskConfirmation with parameter",pId)).
        ErrorStringUtil:AddQuestion(DataBuffer, pid, oQuestion:Message).  
        undo, throw new TransactionError().  
    end method.
    
    method protected logical AskQuestion(pId as integer, pMessage as character):
        define variable oQuestion as IQuestion no-undo.
        if IsQuestionConfirmed(Pid) then 
            return true.
        if DataBuffer:dataset:error   then      
            undo, throw new TransactionError(). 
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message > "" then     
            undo, throw new IllegalOperationError(subst("Use of AskConfirmation with extent parameter or no message parameter for question &1 predefined with message",pId)).
        ErrorStringUtil:AddQuestion(DataBuffer, pid, pMessage).
        undo, throw new TransactionError().
    end method.
    
    method protected logical AskQuestion(pId as integer, pcParams as character extent):
        define variable oQuestion as IQuestion no-undo.
        define variable cMessage  as character no-undo.
        if IsQuestionConfirmed(Pid) then 
            return true.
        if DataBuffer:dataset:error   then    
            undo, throw new TransactionError(). 
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Use of AskConfirmation with non extent message parameter for Question with no predefined message",pId)).
        
        cMessage = Array:SubstituteArray(oQuestion:Message, pcParams).
        ErrorStringUtil:AddQuestion(DataBuffer, pid, cMessage).
        undo, throw new TransactionError().
    end method.
    
    method public void Initialize ():
        
    end method.
    
    method public void Destroy ():
        delete object Datasourcehandle no-error.
    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    destructor public DataSource ( ):
        Destroy().
    end destructor.
    
end class.
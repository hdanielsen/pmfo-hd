 
 /*------------------------------------------------------------------------
    File        : ServiceEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon May 27 11:05:08 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.DataLayer.IServiceDataAccess from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Progress.Reflect.Flags from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Parameter from propath.
using Progress.Reflect.Property from propath.
using Pmfo.Core.Common.WarningMessages from propath.

class Pmfo.Core.BusinessLogic.ServiceEntity inherits DataService implements IServiceEntity abstract: 
    define public abstract property Resource as character no-undo  get.
    define public property IsDeprecated   as logical        no-undo get.  protected set. 
    define public property WarningMessages as IWarningMessages no-undo get. protected set.
    
    define public property ServerType as ServerTypeEnum no-undo 
        get():
            if not valid-object(ServerType) then 
                ServerType = ServerTypeEnum:Default.
            return ServerType.   
        end.
        protected set(pType as ServerTypeEnum):
            AssertConstructorProperty().
            ServerType = pType. 
        end. 
    
    define public property PostMethodNames as character no-undo 
        get.
        protected set(pNames as character):
            AssertConstructorProperty().
            PostMethodNames = pNames. 
        end. 
    
    define public property MethodOnly as logical no-undo 
        get. 
        protected set(plMethodOnly as logical):
            AssertConstructorProperty().
            MethodOnly = plMethodOnly.
        end.  
           
    define public property CanDelete      as logical no-undo 
        get. 
        protected set(plCanDelete as logical):
            AssertConstructorProperty().
            CanDelete = plCanDelete.
           
        end.
         
    define public property CanCreate      as logical no-undo 
        get. 
        protected set(plCanCreate as logical):
            AssertConstructorProperty().
            CanCreate = plCanCreate.
        end. 
        
    define public property CanUpdate      as logical  no-undo 
        get. 
        protected set(plCanUpdate as logical):
            AssertConstructorProperty().
            CanUpdate = plCanUpdate.
        end. 
    
    // support passing parameters instead of body 
    define public property AllowAction as logical no-undo 
        get. 
        protected set(plAllowAction as logical):
            AssertConstructorProperty().
            AllowAction = plAllowAction.
        end. 
    
    define public property Public as logical init true no-undo 
        get.
        protected set(plPublic as logical):
            AssertConstructorProperty().
            Public = plPublic.
        end. 
         
    define public property ClientOnly     as logical init false no-undo get. 
   
    define public property ReadOnly       as logical init ?  no-undo 
        get():
            return CanCreate = false and CanUpdate = false and CanDelete = false.
        end.
            
        
    // defaults to readonly,and thus default to false if any other Can* is true , 
    // to suppprt both GET and POST CanRead must be set to true   
    define public property CanRead      as logical init ? no-undo 
        get():
            // default to read pnly 
           if CanRead = ? then 
               return ReadOnly.
           else
               return CanRead.
        end.
        protected set(plCanRead as logical):
            
            AssertConstructorProperty().
            CanRead = plCanRead.
        end.    
     
    define public property Properties as Progress.Reflect.Property extent no-undo 
        get.
        protected set.
    
    define public property Parameters as IParameter extent no-undo 
        get.
        protected set.
    
    define public property Method as Method no-undo 
        get. 
        protected set.
        
    method public character MethodOperationType(pcMethodName as character).  
/*        if MethodList:ContainsKey(new String(pcMethodName)) then*/
/*        do:                                                     */
            if ReadOnly then 
                return "GET".
            else if CanRead = false then 
                return "POST". 
            else if pcMethodName Begins "Update" then 
                return "PUT".
            else if pcMethodName Begins "Get" then 
                return "GET". 
            else 
                return "POST".
/*        end.*/
/*        undo, throw new IllegalArgumentError(subst("Method '&1' is not found in Service '&2'",pcMethodName,Resource)).*/
    end method. 
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property ExceptList as character no-undo 
    get.
    set. 
    
    define protected property ExceptMethodList as character no-undo 
    get.
    set. 
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ServiceEntity (  ):
        super().
        ExceptList = "Resource,Method".
    end constructor.
    
    constructor protected ServiceEntity (pcExceptlist as char ):
        this-object().
        Assert:NotEmpty(pcExceptList,"Except List").
        Exceptlist = pcExceptList + "," + ExceptList.
    end constructor.
    
    constructor protected ServiceEntity (pcExceptlist as char, pcHideMethods as char ):
        super().
        Exceptlist = left-trim(pcExceptList + "," + ExceptList,",").
        ExceptMethodList = pcHideMethods.
    end constructor.
    
    // subclasses override to control that this is not set when not possible 
    // - if for example the body requires some meta properties  
    method protected void AssertAllowAction():
        if AllowAction and ReadOnly then
            undo, throw new IllegalStateError("AllowAction cannot be set to true for read only Eentity"). 
    end method.    
    
    method protected Flags GetFlags():
        return Progress.Reflect.Flags:Public or Progress.Reflect.Flags:DeclaredOnly or Progress.Reflect.Flags:Instance. 
    end method.      
    
    method protected Flags GetPropertyFlags():
        return GetFlags(). 
    end method. 
    
    method protected Flags GetMethodFlags():
        return GetFlags(). 
    end method. 
    
    method protected Property extent GetProperties() :
        define variable oSet        as ISet no-undo.
        define variable oProperties as Property extent no-undo.
        define variable i as integer no-undo.
       
        oSet = new Set().
        oProperties = this-object:GetClass():GetProperties(GetPropertyFlags()).
        do i = 1 to extent(oProperties):
            // skip objects
            if oProperties[i]:DataTypeName = ? and lookup(oProperties[i]:name,ExceptList) = 0 then
                oSet:Add(oProperties[i]). 
        end.
        return cast(oSet:ToArray(),Property).
    end method.     
    
    method public Method extent GetMethods() :
        define variable oMethods as Method extent no-undo.        
        define variable oPublicMethods as Method extent no-undo.
        define variable iPub as integer no-undo.      
        define variable iInternal as integer no-undo.  
      
        oMethods = this-object:GetClass():GetMethods(GetMethodFlags()).
        
        if ExceptMethodList > "" then 
        do:
            extent(oPublicMethods) = extent(oMethods) - num-entries(ExceptMethodList).
            do iInternal = 1 to extent(oMethods):
                if lookup(oMethods[iInternal]:name,ExceptMethodList) = 0 then
                do:
                    iPub = iPub + 1. 
                    oPublicMethods[iPub] = oMethods[iInternal].
                end.    
            end.
            return oPublicMethods.      
        end.
        else 
            return oMethods.    
        
    end method.  
       
    // IFetchDelegator - NOT in IBusinessEntity
    method public void AddWarning(pcMsg as character):
        if not valid-object(WarningMessages) then
            WarningMessages = new WarningMessages().
         WarningMessages:Add(Resource, pcMsg).
    end method. 
    
    method protected void AddDeprecatedWarningAndLog(pcWarning as character, pcLogMessage as character):
        if pcWarning > "" then
        do:
            if pcLogMessage > "" then
                pcWarning = subst("&1 &2",pcWarning,"A note has been written to the log to inform IT about this.").
            else 
                pcLogMessage = pcWarning.           
            
            AddWarning(pcWarning).
            if session:remote then 
                message pcLogMessage.
            else     
                message pcLogMessage
                        view-as alert-box warning title "Log Message - Written to Log from Remote Session".           
       end.    
        
    end.    
    
    method public override void Initialize():
        define variable i           as integer no-undo.
        define variable oMethods    as Method extent no-undo.
        define variable iParam      as integer no-undo.
        define variable lMethod     as logical no-undo.
        define variable lMethodList as logical no-undo.
        
        if ReadOnly = true and CanRead = false then 
        do: 
            undo, throw new IllegalStateError(subst("&1 CanRead cannot be set to false when ReadOnly is true",Resource)).
        end.
               
        Properties = GetProperties().
        
        if AllowAction then AssertAllowAction().
        
         
        oMethods = GetMethods().
        
        if extent(oMethods) <> ? then
        do:
            do i = 1 to extent(oMethods):
                if oMethods[i]:name = "Execute" then
                do:
                    if lMethod then
                        undo, throw new ApplicationError(subst("Can only have one Execute method in entity &1",this-object:GetClass():TypeName)).
                    
                    Method = oMethods[i].
                    
                    if extent(Properties) = 0 and Method:ReturnType <> Progress.Reflect.DataType:Logical then
                        undo, throw new IllegalStateError("Execute method must return logical if the entity does not have any properties").
                          
                    lMethod = true.
                end.
                // ensure initialize and such is not included 
                else if oMethods[i]:IsOverride = false then
                do:
                    if oMethods[i]:ReturnType = Progress.Reflect.DataType:Object then
                    do:
                        // support json 
                        if oMethods[i]:ReturnTypeName <> "Progress.Json.ObjectModel.JsonObject" then  
                           undo, throw new IllegalStateError(subst("Method &1 in &2 returns an Object. Methods in Service Entity must return a scalar value",oMethods[i]:name,Resource)).
                    end.
                    lMethodList = true.
                end.   
            end.
            
        end.
        if lMethod and lMethodList then
            undo, throw new IllegalStateError(subst("The Service Entity '&1' cannot have Execute if it has other public methods",Resource)).
        
        if lMethod  then
        do: 
            Parameters = GetPublicParameters(Method:GetParameters()).
        end.
                                       
    end. 
    
    method protected IServiceDataAccess CreateDataAccess():
        return CreateDataAccess(this-object:Resource).    
    end.
    
    method protected IParameter extent GetPublicParameters(pParameters as Parameter extent):
        define variable oParameters as IParameter extent no-undo.
        define variable i as integer no-undo.
        extent(oParameters) = extent(pParameters).
        do i = 1 to extent(oParameters):
            if lookup(pParameters[i]:DataType:ToString(),"character,date,datetime,datatimeTZ,decimal,int64,integer,logical,longchar") = 0 then
                undo, throw new IllegalStateError(subst("Parameter &1 is of type &2. All parameters must be primitive.",pParameters[i]:name,pParameters[i]:DataType:ToString())).
            oParameters[i] = new Pmfo.Core.Common.Parameter(
                                       GetPublicParameterName(pParameters[i]:name),
                                       pParameters[i]:DataType:ToString() + if pParameters[i]:extent <> 0 then "Array" else "").
        end.   
        return oParameters. 
    end.     
    
    // public to allow use of reflection to resolve publci name from implementation
    method public character GetPublicParameterName(pcName as character):
        define variable cChr as character no-undo.
        define variable i as integer no-undo.
        if pcName begins "p" then
        do:
            do i = 1 to 3:
                cChr = substr(pcName,i,1).
                if i > 1 and  not compare(cChr,"eq",lc(cChr),"case-sensitive") then
                do:
                    pcName = lc(substr(pcName,i,1)) + substr(pcName,i + 1).
                    leave.
                end.    
            end.
        end.    
        return pcName.
    end method.
    
    method protected IServiceDataAccess CreateDataAccess(pcname as char):
        return cast(Application:ServiceManager,IServiceManager):CreateServiceDataAccess(pcname).    
    end.
    
    method public override void Destroy():
    end method.    
end class.
/*------------------------------------------------------------------------
    File        : StartupManager
    Purpose     : Primary manager for framework startup
    Description : System Startup Manager
    Author(s)   : rosmith
    Created     : Nov 02 2016
    Notes       :
    Updated     :
        S.E. Southwell - 2/28/2020 - git286 - Added new property for environment-specific 
        config directory.
  ----------------------------------------------------------------------*/
 
block-level on error undo, throw.

using Ccs.Common.IManager from propath.
using Ccs.Common.IServiceManager from propath.
using Ccs.Common.ISessionManager from propath.
using Ccs.Common.IStartupManager from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.Map from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Manager.Manager from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Core.Manager.SessionManager from propath.
using Pmfo.Core.Manager.StartupManager from propath.
using Pmfo.Util.FileUtil from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.ISupportInitialize from propath.
using Progress.Lang.Error from propath.

class Pmfo.Core.Manager.StartupManager inherits Manager implements IStartupManager use-widget-pool:
     
    define public static property ConfigDirectory  as char no-undo init "config" get. set.
    define public static property EnvConfigDirectory  as char no-undo 
        get():
            define variable cCatalina as character no-undo.
            // use PAS config , but ensure that the dir is valid also when 
            // running outideof PAS (i.e. unit test)
            cCatalina = os-getenv("CATALINA_BASE").
            if cCatalina > "" then
                EnvConfigDirectory = FileUtil:AppendToPath(cCatalina,"conf/pmfo").
            else     
                EnvConfigDirectory = ConfigDirectory.
            return EnvConfigDirectory.    
        end. 
        set.
    define private static property mInstance as IStartupManager no-undo get. set.
    
    define protected property Managers as IMap no-undo 
        get():
            if not valid-object(Managers) then
                Managers = new Map().
            return Managers.    
        end. 
        set. 
/*    
    define private temp-table ttGeneral no-undo serialize-name "General"
        field iStopAfter as integer serialize-name "Timeout".
*/
    define private temp-table ttImplMapping no-undo serialize-name "ManagerMapping"
        field Manager        as character
        field Implementation as character
        index ids as primary unique Manager.

    define private dataset dsConfig serialize-name "Config"
        for ttImplMapping.

    /* The Static instance object used to instantiate a singleton. */
    define public static property Instance as IStartupManager no-undo
        get():
            if not valid-object(mInstance) then
            do:
                mInstance = new StartupManager().
                Instance  = mInstance.
            end.
            return Instance.
        end get.
        private set.

    /* The STOP-AFTER value used to set a time-out value for each interface request. */
    define public property interfaceStopAfter as integer initial 20 no-undo
        get.
        private set.

    constructor private StartupManager ( ):
        // set as early as possible so static constructor in dataservie does not kick in when execiuted from startlib. 
        Application:StartupManager = this-object.
        initialize().
    end constructor.

    destructor public StartupManager ( ):
        dispose().
    end destructor.

    method override public void initialize ( ):
        loadConfig().
        startManagers().
    end method.

    method override public void dispose ( ):
        delete object mInstance no-error. /* Destroy instance. */
    end method.

    /* Loads the Config JSON which defines which manager implementations to initialize. */
    method protected void loadConfig ( ):
        define variable cFullCfgFileName as character no-undo.
        file-info:file-name = FileUtil:AppendToPath(ConfigDirectory,"startup.json").
        cFullCfgFileName = file-info:full-pathname.
        if cFullCfgFileName ne ? then
            dataset dsConfig:read-json("FILE", cFullCfgFileName, "EMPTY").
        else
            undo, throw new ApplicationError(substitute("Required config file [startup.json] not found in [&1]", ConfigDirectory)).

/*        find first ttGeneral no-lock no-error.                    */
/*        if available(ttGeneral) and ttGeneral.iStopAfter ne 0 then*/
/*            interfaceStopAfter = ttGeneral.iStopAfter.            */

        if session:debug-alert or session:error-stack-trace then
            interfaceStopAfter = -1.
    end method.

    /* Instantiate and initialize all configured managers. */
    method private void startManagers ( ):
        define variable oIterator as IIterator no-undo.
        define variable oManager  as Object no-undo.
        define variable oInnerError as Error no-undo.
        checkAllManagersAreListed().
        
        for each ttImplMapping:
            startManager(ttImplMapping.Manager, ttImplMapping.Implementation).
            catch err as Progress.Lang.Error:
                if session:remote then
                    message substitute("Error while starting manager '&1': &2", oManager:GetClass():TypeName, err:GetMessage(1)).
                else undo, throw err.
            end catch.
        end.
        oIterator = Managers:Values:Iterator().
        do while oIterator:HasNext() on error undo, throw.
            oManager = OIterator:Next().
            if type-of(oManager,IManager) then
                cast(oManager,IManager):initialize().
            else if type-of(oManager,ISupportInitialize) then
                cast(oManager,ISupportInitialize):initialize().
            catch err as Progress.Lang.Error:
                if session:remote then
                do:
                    message substitute("Error while initializing manager '&1': &2", oManager:GetClass():TypeName, err:GetMessage(1)).
                    if type-of(err,ApplicationError) then
                    do:
                        if valid-object(cast(err,ApplicationError):InnerError) then
                        do:
                            oInnerError = cast(err,ApplicationError):InnerError.
                            message 
                                "Caused by: "  oInnerError:GetMessage(1) skip
                                "Call Stack: " oInnerError:CallStack 
                             .
                        end.
                    end.     
                end.    
                else undo, throw err.
            end catch.
        end.  
        
    end method.

    method private void checkAllManagersAreListed ( ):
        /* CCS Managers (Required) */
        checkManagerIsListed(get-class(IServiceManager):TypeName, get-class(ServiceManager):TypeName).
        checkManagerIsListed(get-class(ISessionManager):TypeName, get-class(SessionManager):TypeName).
/*                                                                                                                                                              */
/*        /* Application Managers (Required by default, unless set as optional) */                                                                              */
/*        checkManagerIsListed(get-class(Spark.Core.Manager.IConnectionManager):TypeName, get-class(Spark.Core.Manager.ConnectionManager):TypeName).            */
/*        checkManagerIsListed(get-class(Spark.Core.Manager.ILoggingManager):TypeName, get-class(Spark.Core.Manager.LoggingManager):TypeName).                  */
/*        checkManagerIsListed(get-class(Spark.Core.Manager.IStateManager):TypeName, get-class(Spark.Core.Manager.StateManager):TypeName).                      */
/*        checkManagerIsListed(get-class(Spark.Core.Manager.ITranslationManager):TypeName, get-class(Spark.Core.Manager.TranslationManager):TypeName).          */
/*        if checkManagerIsListed(get-class(Spark.Core.Manager.ICatalogManager):TypeName, get-class(Spark.Core.Manager.CatalogManager):TypeName, false) then do:*/
/*            /* If the CatalogManager is present, then make sure the supporting SchemaManager is listed as well. */                                            */
/*            checkManagerIsListed(get-class(Spark.Core.Manager.ISchemaManager):TypeName, get-class(Spark.Core.Manager.SchemaManager):TypeName).                */
/*        end. /* CatalogManager Listed */                                                                                                                      */
    end method.

    method private logical checkManagerIsListed ( input pcManager        as character,
                                               input pcImplementation as character ):
        /* Check if manager is listed, otherwise create as required by default. */
        return this-object:checkManagerIsListed(pcManager, pcImplementation, true).
    end method.

    method private logical checkManagerIsListed ( input pcManager        as character,
                                                  input pcImplementation as character,
                                                  input plIsRequired     as logical ):
        find ttImplMapping
             where ttImplMapping.Manager eq pcManager no-error.
        if not available(ttImplMapping) and plIsRequired then do:
            /* Create any missing, required manager implementations. */
            create ttImplMapping.
            assign
                ttImplMapping.Manager        = pcManager
                ttImplMapping.Implementation = pcImplementation
                .
        end.
        return available(ttImplMapping).
    end method.

    method private void startManager ( input pcManager        as character,
                                       input pcImplementation as character ):

        define variable oManagerType        as Progress.Lang.Class no-undo.
        define variable oImplementationType as Progress.Lang.Class no-undo.
        define variable oInstance           as Object no-undo.
 
        oManagerType = Progress.Lang.Class:GetClass(pcManager) no-error.
        if not valid-object(oManagerType) then
            undo, throw new ApplicationError(substitute("Invalid Manager [&1]", pcManager)).

        oImplementationType = Progress.Lang.Class:GetClass(pcImplementation) no-error.
        if not valid-object(oImplementationType) then
            undo, throw new ApplicationError(substitute("Invalid Manager Implementation [&1]", pcImplementation)).
        
        oInstance = oImplementationType:New().
      
        if not type-of(oInstance,IManager) then
            undo, throw new ApplicationError(substitute("Manager [&1] does not implement [&2]", pcImplementation,get-class(IManager):TypeName)).
       
        Managers:Put(oManagerType,oInstance).
        /*Assign the Application:ServiceManager property here so that it is available for other managers as soon as it has been started*/
        if oManagerType:IsA(get-class(Ccs.Common.IServiceManager)) then
            Ccs.Common.Application:ServiceManager = cast(oInstance, Ccs.Common.IServiceManager).

        /*Assign the Application:SessionManager property here so that it is available for other managers as soon as it has been started*/
        if oManagerType:IsA(get-class(Ccs.Common.ISessionManager)) then
            Ccs.Common.Application:SessionManager = cast(oInstance, Ccs.Common.ISessionManager).
        
    end method.

    method private void stopManager ( input pcManager as character ):
        define variable oManager as Object no-undo. 
        
        oManager = Managers:Get(Progress.Lang.Class:GetClass(pcManager)).
        if valid-object(oManager) then do:
            if type-of(oManager,IManager) then
            do:
                cast(oManager,IManager):dispose().
            end.
            /* Delete the reference to the Object and the garbage collection will remove the object. */
            Managers:Remove(oManager:GetClass()).
        end.
    end method.

    /*------------------------------------------------------------------------------
        Purpose: Retrieve an instance of the specified IManager object.
        Notes:
        @param pServiceType The Progress.Lang.Class repersenting the required service.
        @return IManager implementation of the requested type, or ? if its not configured.
        ------------------------------------------------------------------------------*/
    method public IManager getManager ( input poServiceType as Progress.Lang.Class ):

        define variable oManager as Object no-undo. 
         
        oManager = Managers:Get(poServiceType).  
        
        if not valid-object(oManager) then
            undo, throw new ApplicationError(substitute("Invalid Manager Type [&1]", poServiceType:TypeName)).
        
        return cast(oManager, IManager).

        catch err as Progress.Lang.Error:
            if log-manager:logging-level ge 3 then
                log-manager:write-message(err:GetMessage(1), "GET-MGR").
            return ?. /* Return unknown instead of throwing error higher. */
        end catch.
    end method.

    /* Purpose: Stop all non-essential application managers. */
    method public void stopManagers ( ):
        define variable oIterator    as IIterator no-undo.
        define variable oManagerType as Progress.Lang.Class no-undo.
        oIterator = Managers:KeySet:Iterator().
        do while oIterator:HasNext():
            oManagerType = cast(oIterator:Next(),Progress.Lang.Class).
            if not (oManagerType:IsA(get-class(Ccs.Common.IServiceManager)) 
                    and
                    oManagerType:IsA(get-class(Ccs.Common.ISessionManager))
                    ) then
            do:
                stopManager(oManagerType:TypeName).
            end. 
        end.    
    end method.

end class.

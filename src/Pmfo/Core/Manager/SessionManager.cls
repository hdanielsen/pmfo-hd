 
 /*------------------------------------------------------------------------
    File        : SessionManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 15 10:20:36 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Ccs.Common.IClientContext from propath.
using OpenEdge.Core.Session from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Manager.ISessionManager from propath.
using Pmfo.Core.Manager.Manager from propath.
using Progress.ApplicationServer.AdapterTypes from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using OpenEdge.Core.DateTimeAddIntervalEnum from propath.

class Pmfo.Core.Manager.SessionManager inherits Manager implements ISessionManager : 
    
    define public property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
    
    define public property CurrentClientContext as IClientContext no-undo 
    get.
    set. 
    
    define public property QueryTimeout as integer no-undo 
    get.
    set. 
    
    define public property IsDebug as logical no-undo 
    get. 
    set. 
    
    define property LastInterval as datetime no-undo get. set. 
    
    // Thse are textual values not inntended for logic other than logging 
    // including assignment of table fields that keep track of user stuff 
    // subclasses should set for nmore detailed info like for example "Mobile" 
    define public property ClientType  as character no-undo 
        get():
            if ClientType = "" then
            do:
                // defaults 
                if valid-object(session:current-request-info) then
                do:
                    case session:current-request-info:AdapterType:
                        when AdapterTypes:APSV then
                            ClientType = "Appserver".
                        when AdapterTypes:WEB then
                            ClientType = "Web".
                        when AdapterTypes:SOAP then
                            ClientType = "Soap".
                        when AdapterTypes:REST then
                            ClientType = "Rest".
                    end.    
                end.  
                else 
                    ClientType = "desktop".
                      
            end.
            return ClientType.    
        end.     
        protected set.
    
    method public void endRequestEnvironment(  ):
        undo, throw new NotImplementedError().
    end method.

    method public void establishRequestEnvironment( input phClientPrincipal as handle ):
        undo, throw new NotImplementedError().
    end method.

    method public void establishRequestEnvironment( input pcSessionID as character ):
        undo, throw new NotImplementedError().
    end method.
    
    method public void Authorize(pcUrl as character):
        undo, throw new NotImplementedError().
    end method.     
    
    method public override void initialize(  ):
        ImportConfig( ).
        // 11.7.5 (and before?) has bug in OpenEdge version so add Pmfo version here
       // LoggerFilterRegistry:Registry:Put(LoggerFilterRegistry:ERROR_FORMAT,get-class(ErrorFormat)).
    end method.
    
    method public override void dispose(  ):
    end method.
    
    method public void Destroy(  ):
        dispose().
    end method.
     
    method protected void ImportConfig(  ):
        define variable oJson as JsonObject no-undo.
        oJson   = LoadConfig("session.json").
        QueryTimeout = oJson:GetInteger("QueryTimeout").
        return.
    end method.  
    
    method void LogInterval(pctext as character).
        define variable dt as datetime no-undo.
        dt = now.
        if session:remote then
        message subst("Log Interval: &1 &2",
                      pcText,
                      if LastInterval = ? then 0 else interval(dt,LastInterval,DateTimeAddIntervalEnum:Milliseconds:ToString())) 
        .
        LastInterval = dt.
    end method. 
    
    method void ResetLogInterval().
        LastInterval = ?.
    end method.  
end class.
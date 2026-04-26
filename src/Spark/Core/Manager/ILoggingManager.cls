using Ccs.Common.IManager from propath.
using OpenEdge.Web.IWebRequest from propath.

//using Spark.Core.Util.ErrorTypeEnum from propath.

/**
 * Logging Manager Interface
 */
interface Spark.Core.Manager.ILoggingManager inherits IManager:

    // performance attribute - set to true if any type of logging is activated for the 
    // web request  
    define public property IsWebRequestLogActive as logical no-undo get. set.
    define public property LogEveryWebRequest    as logical no-undo get. set.
    define public property ProfileEveryWebRequest    as logical no-undo get. set.
    define public property DBIfilesize as logical no-undo get. set.
    define public property DBIABLApp as character no-undo get. set.
    method public void LogWebRequest(pWebRequest as IWebRequest ).
    method public void LogWebRequestToSeq(pWebRequest as IWebRequest ).
    method public void LogDBIFileSizeToSeq(pcWebAppName as character).
    method public void EnableProfilerWebRequest(pWebRequest as IWebRequest ).
    method public void DisableProfilerWebRequest().


    /**
     * Log a message to the appserver logfile.
     * NOTE: This method will be defined in the LogMessate.i include, so take care if modifications are made to that file.
     */
    method public void logMessage( pcMessage as character, pcSubSystem as character, piLogLevel as integer).

    /**
     * Log a caught error to the appserver logfile.
     * NOTE: This method will be defined in the LogMessate.i include, so take care if modifications are made to that file.
     */
    method public void logError( input pcContextMessage as character, input poErr as Progress.Lang.Error, input pcSubSystem as character, input piLogLevel as integer).

    method public integer updateLoggingLevel ( ).

    method public character updateLogEntryTypes ( ).

end interface.
 
 /*------------------------------------------------------------------------
    File        : MemoryLogger
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 07 07:00:47 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Web.IWebRequest from propath.
using Ccs.Common.Application from propath.


using Pmfo.Core.BusinessLogic.MemoryObjectBE from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.IMemoryLogger from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.
    

class Pmfo.Util.MemoryLogger abstract implements IMemoryLogger: 
       
    {Pmfo/Core/schema/memoryObject.i}
    define temp-table bttMemoryObject like ttMemoryObject.
    
    define protected property MemoryObjectsFileName as character no-undo get. set.
    
    define protected property MEMORYOBJECTS_FIXEDNAME as character init "memoryobjects" no-undo get.
    
    define protected property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
    
    define protected property MemoryObject as MemoryObjectBE no-undo
        get():
            if not valid-object(MemoryObject) then 
                MemoryObject = cast(ServiceManager:CreateBusinessEntity("memoryObjects"),MemoryObjectBE).
            return MemoryObject.    
        end.
        set.    
    
     method public void Log(poWebRequest as IWebRequest):
         define variable dtRequestTime as datetime no-undo.
         if MemoryObjectsFileName = "" then 
         do:
             MemoryObjectsFileName = subst("&1/&2-&3.log",
                                           right-trim(session:temp-dir,if opsys = "unix" then "/" else "~\"),
                                           MEMORYOBJECTS_FIXEDNAME,
                                           session:current-request-info:SessionID
                                           ).
                          
             // create an empty file when first called for the session                              
             output to value(MemoryObjectsFileName) unbuffered no-echo.
             output close.
         end.
         dtRequestTime = now.
         MemoryObject:Clear().
         
         MemoryObject:GetData(output table bttMemoryObject).
         
         for each bttMemoryObject :
             find ttMemoryObject where ttMemoryObject.handle = bttMemoryObject.handle no-error.
             if not avail ttMemoryObject then
             do:
                 create ttMemoryObject.
                 buffer ttMemoryObject:buffer-copy(buffer bttMemoryObject:handle).
                 LogLeak(buffer ttMemoryObject,
                         dtRequestTime,
                         if valid-object(poWebRequest) then poWebRequest:URI:ToString() else "").
                
             end.    
         end.      
     end method.
         
/*     method public void LoadConfig(pJson as JsonObject):                                                                                                               */
/*         define variable cPath as character no-undo.                                                                                                                   */
/*                                                                                                                                                                       */
/*                                                                                                                                                                       */
/*         MemoryObjectsFileName = replace(pJson:GetCharacter("MemoryObjectsFileName"),"~\","/").                                                                        */
/*                                                                                                                                                                       */
/*         if index("/",MemoryObjectsFileName) > 0 then                                                                                                                  */
/*         do:                                                                                                                                                           */
/*             cPath = StringUtil:RemoveEntryFromList(num-entries("/",MemoryObjectsFileName),MemoryObjectsFileName,"/").                                                 */
/*             file-info:file-name = cPath.                                                                                                                              */
/*             cPath = file-info:full-pathname.                                                                                                                          */
/*             if index("D",file-info:file-type) = 0 then                                                                                                                */
/*             do:                                                                                                                                                       */
/*                 message subst("The MemoryObjectsFileName '&1' in the logging config is not valid. The Path '&2' is not a directory. Memory Logging is not activated.",*/
/*                                pJson:GetCharacter(MemoryObjectsFileName),                                                                                             */
/*                                cPath).                                                                                                                                */
/*             end.                                                                                                                                                      */
/*             if index("W",file-info:file-type) = 0 then                                                                                                                */
/*             do:                                                                                                                                                       */
/*                 message subst("The Path '&2' of The MemoryObjectsFileName '&1' is not a writeable directory. Memory Logging is not activated.",                       */
/*                                cPath,                                                                                                                                 */
/*                                pJson:GetCharacter(MemoryObjectsFileName)                                                                                              */
/*                                ).                                                                                                                                     */
/*             end.                                                                                                                                                      */
/*         end.                                                                                                                                                          */
/*         else                                                                                                                                                          */
/*         do:                                                                                                                                                           */
/*             MemoryObjectsFileName = subst("&1/&2",                                                                                                                    */
/*                                           right-trim(session:temp-dir,if opsys = "unix" then "/" else "~\"),                                                          */
/*                                           MemoryObjectsFileName).                                                                                                     */
/*                                                                                                                                                                       */
/*         end.                                                                                                                                                          */
/*         // create new log file                                                                                                                                        */
/*         output to value(MemoryObjectsFileName) unbuffered no-echo.                                                                                                    */
/*         output close.                                                                                                                                                 */
/*         this-object:Log(?).                                                                                                                                           */
/*                                                                                                                                                                       */
/*     end method.*/
         
     method protected void LogLeak(buffer ttMemoryObject for ttMemoryObject,pTime as datetime,pcURi as character):
         output to value(MemoryObjectsFileName) unbuffered no-echo append.
/*         find ttMemoryLeakLog where ttMemoryLeakLog.contextId = ContextId no-error.                               */
/*         output to value(ttMemoryLeakLog.logFile) unbuffered append.                                              */
         put unformatted pTime " " pcURi subst(" Session: &1 &2: &3 &4 &5 Parent: &6 Info: &7",
                               session:current-request-info:SessionID,
                               ttMemoryObject.type,
                               ttMemoryObject.handle,
                               ttMemoryObject.name,
                               ttMemoryObject.publicName,
                               ttMemoryObject.parentHandle,
                               ttMemoryObject.info
                               )
                           skip.

          
         finally:
             output close.
         end.  
     end method.   
     
         
     
       
end class.
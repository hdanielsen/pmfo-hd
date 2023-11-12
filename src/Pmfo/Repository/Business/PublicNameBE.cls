 
 /*------------------------------------------------------------------------
    File        : PublicNameBE
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 23 11:18:29 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Session from propath.
using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.BusinessLogic.SchemaTypeEnum from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.FileUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.

class Pmfo.Repository.Business.PublicNameBE abstract inherits BusinessEntity: 
    &scop mode protected
    {Pmfo\Repository\Business\defs\publicnames-dataset.i}
    &undefine mode
    
    constructor PublicNameBE ():
        super(dataset dsPublicName:handle).
        ClientOnly  = true.
      //  Public = false.
    end constructor.  
    
    method public character GetMappedName(pCategory as SchemaTypeEnum, pSourceName as character):
/*        if AllDataIsFetched = false then*/
/*            GetData().                  */
        case pCategory:
            when SchemaTypeEnum:Table then 
                return FindTable(pCategory,pSourceName). 
            when SchemaTypeEnum:CodeTable then 
                return FindTable(pCategory,pSourceName).
            when SchemaTypeEnum:Field then 
                return FindField(pSourceName).
            otherwise 
                undo, throw new UnsupportedOperationError(subst("GetMappedName for SchemaTypeEnum:&1",pCategory:ToString())).      
       end case.    
    end method.        
    
    method protected character GetConfigFullPath():
        define variable oJson as JsonObject no-undo.
        
        define variable cConfigFile as character no-undo.
      
        file-info:file-name = Pmfo.Core.Manager.StartupManager:ConfigDirectory.  
        
        if file-info:full-pathname = ? then 
            undo, throw new IllegalOperationError(subst("Config Directory [&1] not found in propath",Pmfo.Core.Manager.StartupManager:ConfigDirectory)).
        
        cConfigfile = FileUtil:AppendToPath(file-info:full-pathname,"publicNames.json").
        if search(cConfigfile) = ? then
            undo, throw new IllegalOperationError(subst("Public Names Config File [&1] not found",cConfigfile)).
         
        return cConfigFile.

    end method.
    
    method override protected void ReceiveData( input pParent as Pmfo.Core.BusinessLogic.IBusinessEntity, input pReceiveDataArgs as Pmfo.Core.BusinessLogic.IReceiveDataEventArgs, input phDataset as handle ):
        LoadData().
    end method.
    
    method protected void LoadData():
        define variable oError as Error no-undo.
        do on error undo, throw:
            dataset dsPublicName:read-json ("file",GetConfigFullPath(),"append").
            catch e as Progress.Lang.Error :
                if e:GetMessagenum(1) = 132 then
                do:
                    undo, throw new IllegalStateError(GetMessage(e)). 
                end.   
            end catch.
        end.
        
    end method.
    
    method protected character GetMessage(pe as Error):
        define variable cMessage as character no-undo.
        define variable cQuotedValues as character extent no-undo. 
        
        if pe:GetMessagenum(1) = 132 then 
        do:  
             cMessage = trim(pe:GetMessage(1),"* ").
             if cMessage begins buffer ttentityname:name then 
             do: 
                 StringUtil:MaskQuotes(cMessage, "@", output cQuotedValues).
                 if extent(cQuotedValues) = 2 then 
                 do:
                     return  subst('More than one entry found for Collection "&1" with category &2 and sourceName &3',
                                    buffer ttentityname:serialize-name,
                                    cQuotedValues[1],
                                    cQuotedValues[2]
                                    ). 
                     
                 end.
                 else if extent(cQuotedValues) = 1 then 
                 do:
                     return  subst('More than one entry found in Collection "&1" with name &2',
                                    buffer ttentityname:serialize-name,
                                    cQuotedValues[1]
                                    ). 
                 end. 
             end.
             else if cMessage begins buffer ttpropertyname:name then 
                 return replace(cMessage,buffer ttpropertyname:name,subst('Collection "&1"',buffer ttpropertyname:serialize-name)).
        end.
        return pe:GetMessage(1).
        catch e as Progress.Lang.Error :
             // if anything goes wrong in the message extraction just return the input 
            return pe:GetMessage(1).
        end catch.
    end method.    
    
    method protected character FindTable(pCategory as SchemaTypeEnum, pcSourceName as character) :
        find ttEntityName where ttEntityName.category   = pCategory:ToString()
                            and ttEntityName.sourceName = pcSourceName no-error.
        if avail ttentityName then 
            return ttEntityName.name.
        return "".                         
    end method.     
    
    method public void ResortStorage() :
         define variable cTempfile as character no-undo.
         define variable cBackup as character no-undo.
         define variable iError as integer no-undo.
         
         cTempFile = fileUtil:GetFileName(session:temp-directory, "publicNames","json").
         cBackup = fileUtil:GetFileName(session:temp-directory, "publicNames","json").
         os-copy value(GetConfigFullPath()) value(cBackup). 
         
         if os-error <> 0 then 
         do: 
             undo, throw new IllegalStateError(subst("Resort aborted. Cannot create a backup file in directory &1",session:temp-directory)).
         end.        
         dataset dsPublicName:write-json ("file",cTempfile,yes).
         
         if search(cTempFile) <> ? then 
         do:
             os-copy value(cTempfile) value(GetConfigFullPath()). 
             iError = os-error.
             if iError <> 0 then 
             do:
                 undo, throw new IllegalStateError(subst("Copy operation failed with OS ERROR &1. The &2 file is damaged. Please restore from SCM or backup file &3",
                                                          iError,
                                                          GetConfigFullPath(),
                                                          cBackup
                                                         )
                                                   ).                  
             end.     
         end.
         finally:
             os-delete value(cTempfile).
             if iError = 0 then
                os-delete value(cBackup).         
         end finally. 
                   
    end method.   
    
    method protected character FindField(pcSourceName as character) :
        find ttPropertyName where ttPropertyName.sourceName = pcSourcename no-error.
        if avail ttPropertyName then 
            return ttPropertyName.name.      
        return "".                   
    end method.     
    
    method public override void Initialize():
        GetData().
    end method.    
end class.
 
 
 /*------------------------------------------------------------------------
    File        : NameUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 27 19:07:15 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Common.INameService from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.ISupportInitialize from propath.
using Pmfo.Repository.Business.PublicNameBE from propath.
using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.SchemaTypeEnum from propath.
using OpenEdge.Web.SendExceptionError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.ApplicationError from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.NameService implements INameService, ISupportInitialize: 

    define private variable PublicNameBE as PublicNameBE no-undo.

    /** @TODO the entityName is currentky assigned in ResourceModel:AfterRow use this instead */  
    method public character EntityName(pcName as char):
        define variable lSpacenamelogic as logical no-undo.
        
        if (pcName > "") = false  then undo, throw new IllegalArgumentError("Parameter name cannot be blank or null").
        
        lSpaceNameLogic = index(pcName,StringConstant:SPACE) > 0 
                        or index(pcName,StringConstant:BACKSLASH) > 0
                        or index(pcName,StringConstant:SEMICOLON) > 0
                        or index(pcName,StringConstant:SINGLE_QUOTE) > 0
                        or index(pcName,StringConstant:DOUBLE_QUOTE) > 0
                        or index(pcName,".") > 0
                        or index(pcName,"=") > 0
                        or index(pcName,"?") > 0
                        or index(pcName,"/") > 0.  
   
        return PublicEntityName(pcName,lSpaceNameLogic).  
    end method.
    
    method protected character PublicEntityName(pcName as char,plUseSpaceNameLogic as logical):
        if plUseSpaceNameLogic then
            pcName = NiceSpacedName(pcName).
        else
            pcName = NiceTableName(pcName).
        pcName = lc(substr(pcName,1,1)) + substr(pcName,2).
        return PluralizeName(pcName).  
    end method.
        
    method public character PublicFieldName(pcName as char):
        return PublicFieldName("",pcname). 
    end method.
    
    method public character PublicFieldName(pcTableName as character,pcName as char):
        if (pcName > "") = false then 
            undo, throw new IllegalArgumentError("Parameter name cannot be blank or null").
        
        pcName = NiceFieldName(pcTableName,pcName).
        pcName = lc(substr(pcName,1,1)) + substr(pcName,2).
        return pcName.  
    end method.
    
    method public character ClassName(pcName as char):
        return ClassName(pcName,false).
    end method.
    
    method public character ClassName(pcName as char,plUseSpaceNameLogic as logical):
        if (pcName > "") = false  then undo, throw new IllegalArgumentError("Parameter name cannot be blank or null").
        
        if plUseSpaceNameLogic then
            pcName = NiceSpacedName(pcName).
        else
            pcName = NiceTableName(pcName).
        pcName = caps(substr(pcName,1,1)) + substr(pcName,2).
        return pcName.
    end method.
    
    method protected character NiceName(pcName as char):
        return NiceName(pcName,false).
    end method.  
     
    method protected character NiceName(pcName as char,plNoCasing as logical):
        pcname = StripHyphens(pcName,plNoCasing).
        pcName = StringUtil:stripBadChars(pcName).
        return pcName.  
    end method.    
    
    method protected character FixSpacedName(pcName as character):
        return pcName. 
    end method. 
    
    method protected character FixTableName(pcName as character):
        return pcName. 
    end method. 
    
    // pcTablename is optional -  will have a value from tooling  
    method protected character FixFieldName(pcTableName as character,pcName as character):
        return pcName. 
    end method. 
    
    method protected final character NiceSpacedName(pcName as char):
        define variable cName as character no-undo.
        define variable lNoCasing as logical no-undo.
        
        define variable cMappedName as character no-undo.
        cMappedName = PublicNameBE:GetMappedName(SchemaTypeEnum:CodeTable, pcName).
        if cMappedName > "" then
            return cMappedName.
        else 
        do:
            pcName = FixSpacedName(pcName). 
            cName = pcName.
            pcName = StripSpaces(pcName).
            if cName <> pcName then 
                lNoCasing = true.
            // TODO remove this dependency in subclass data 
            // as of current some table mappings have dashes in the sourcename     
            cMappedName = PublicNameBE:GetMappedName(SchemaTypeEnum:Table, pcName).
            if cMappedName > "" then
                return cMappedName.
            pcName = StripSlashes(pcName).
            pcName = StripParenthesis(pcName).
            pcname = StripDivider(pcName,"="). 
            pcname = StripDivider(pcName,":"). 
            pcname = StripDivider(pcName,"."). 
            pcname = replace(pcName,"""","").
            pcname = replace(pcName,"'","").
            pcname = replace(pcName,"?","").
            pcName = NiceTableName(pcName,lNoCasing).
            return pcName.
        end.
    end method.   
    
    method protected final character NiceTableName(pcName as char):
        return NiceTableName(pcName,false).
    end method.  
    
    method protected final character NiceTableName(pcName as char,plNoCasing as logical):
        // NiceSpacedName needs this also
           
        define variable cMappedName as character no-undo.
        cMappedName = PublicNameBE:GetMappedName(SchemaTypeEnum:Table, pcName).
        if cMappedName > "" then
            return cMappedName.
        else do:
            pcName = FixTableName(pcName). 
            return NiceName(pcName,plNoCasing).
        end.
    end method.  
    
    method protected final character NiceFieldName(pcTableName as character,pcName as char):
        define variable cMappedName as character no-undo.
        cMappedName = PublicNameBE:GetMappedName(SchemaTypeEnum:Field, pcName).
        if cMappedName > "" then
            return cMappedName.
        else do:
            pcName = FixFieldName(pcTableName,pcName).
            return NiceName(pcName).
        end. 
    end method.   
    
    method public character PluralizeName(pcName as char):
        if (pcName > "") = false  then undo, throw new IllegalArgumentError("Parameter name cannot be blank or null").
        
        if pcName matches "*data" then
        do:
            // nothing  
        end.   
        else if pcname matches "*sh" or pcname matches "*tch"  then
        do: 
            pcName = pcName + "es".
        end.
        else if substr(pcName,length(pcName),1) = "s" then
        do: 
           if pcname matches "*address" or pcName matches "*process" or pcName matches "*flatness" or pcname matches "*status" then
               pcName = pcName + "es".
        end.
                                                       // bay and pay does not follow this rule
        else if substr(pcName,length(pcName),1) = "y" and (pcName matches "*ay") = false   then
        do: 
            pcName = substr(pcName,1,length(pcName) - 1) + "ies".
        end.
        else if substr(pcName,length(pcName),1) = "x" then
        do: 
            pcName = pcName + "es".
        end.
        else do:
            pcName = pcName + "s". 
        end.        
        return pcName.  
    end method.    
    
    method public logical isTableNotInUse(pcTableName as char):
        if (pcTableName > "") = false  then undo, throw new IllegalArgumentError("Parameter name cannot be blank or null").
        return false.
    end method.
    
    method protected character StripHyphens(pcName as char):
        return StripHyphens(pcName,false).
    end method. 
    
    method protected character StripHyphens(pcName as char, plNoCasing as logical):
        define variable i as integer no-undo.
        define variable cNewName as character no-undo.
        define variable cWord as character no-undo.
        define variable lshow as logical no-undo.
        do i = 1 to num-entries(pcName,"-").
            cword = entry(i,pcName,"-").
            cNewName = if i = 1 then cWord 
                       else 
                           cNewName 
                           +  caps(substr(cword,1,1)) + if plNoCasing then substr(cWord,2) else lc(substr(cWord,2)). 
             
        end.
        
        return cNewName.
    end method.     
    
    method protected character StripSpaces(pcName as char):
        define variable i as integer no-undo.
        define variable cNewName as character no-undo.
        define variable cWord as character no-undo.
        if index(pcname," ") > 0 then 
        do:
            do i = 1 to num-entries(pcName," ").
                cword = entry(i,pcName," ").
                cNewName = if i = 1 then lc(cWord) 
                           else cNewName + caps(substr(cword,1,1)) + lc(substr(cWord,2)). 
                
            end.
            return cNewName.
        end.
        else 
            return pcname.
    end method.   
    
    method protected character StripParenthesis(pcName as char):
       
        define variable ileft as int no-undo.
        define variable iright as int no-undo.
        define variable cWord as character no-undo.
       
        ileft  = index(pcName,"(").
        iright  = index(pcName,")").
        if ileft > 0 then do:
            if iright > ileft then  
            do:
                cWord = substr(pcName,ileft + 1,iRight - iLeft - 1).
                 
                pcname = substr(pcName,1,ileft - 1) + caps(substr(cWord,1,1)) + substr(cWord,2) + substr(pcName,iright + 1).
            end.
            else do: 
                pcname = replace(pcName,"(","").
                if iright > 0 then
                     pcname = replace(pcName,")","").
            end.        
        end.
        return pcName.    
    end method. 
    
    
    method protected character StripSlashes(pcName as char):
        return StripDivider(pcName,"/"). 
    end method.         
    
    method protected character StripDivider(pcName as char,pcchar as char):
        define variable i as integer no-undo.
        define variable cNewName as character no-undo.
        define variable cWord as character no-undo.
        
        if index(pcName,pcChar) > 0 then
        do:
            do i = 1 to num-entries(pcName,pcChar).
                cword = entry(i,pcName,pcChar).
                cNewName = if i = 1 then cWord 
                           else cNewName + caps(substr(cword,1,1)) + substr(cWord,2). 
            end.
            return cNewName.
        end.
        else 
           return pcName. 
    end method.         
    
    method public void Destroy(  ):
         
    end method.
    
    method public void Initialize(  ):
        PublicNameBE = cast(Application:ServiceManager:getService(get-class(IBusinessEntity),"publicNames"),PublicNameBE).
        catch e as SendExceptionError :
            if session:remote = false and valid-object(e:InnerError) then
            do: 
                if type-of(e:InnerError,ApplicationError) then 
                do:
                    undo, throw new IllegalStateError(subst("Load of data for Name Service failed with error: &1",
                                                            cast(e:InnerError,ApplicationError):GetStrippedMessage())
                                                         ).                
                 
                end.    
                undo, throw e:InnerError.
            end.     
            undo, throw e.    
        end catch. 
        
    end method.
     
end class.
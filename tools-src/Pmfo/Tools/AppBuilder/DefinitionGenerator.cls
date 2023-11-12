 
 /*------------------------------------------------------------------------
    File        : DefinitionGenerator
    Purpose     : Generate definition include
    Syntax      : 
    Description : Used by Pmfo/Tools/WinDefintion.w
    Author(s)   : hdaniels
    Created     : Sat Jun 27 21:42:56 EDT 2020
    Notes       : This exists to generate include for very large tables 
                  like spec. Too big for the current WinDesign
                  The class reuses CodeGenerator methods 
                  (Should probably have been other way around??)
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.DataAdmin.DataAdminService from propath.
using OpenEdge.DataAdmin.IDataAdminService from propath.
using OpenEdge.DataAdmin.IIndexSet from propath.
using OpenEdge.DataAdmin.ITable from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Tools.AppBuilder.CodeGenerator from propath.
using Pmfo.Tools.Gui.Model.ResourceModel from propath.

class Pmfo.Tools.AppBuilder.DefinitionGenerator: 
    
    define public property NameService   as INameService no-undo get. set.
    define public property CodeGenerator as CodeGenerator no-undo get. set.
    define public property OutputDir     as character no-undo get. set.
    define public property DefinitionDir as char no-undo 
       get:
           return CodeGenerator:DefinitionDir.
       end.    
    
    method public character GetIncludeName(pcTable as character ):
         define variable className as character no-undo. 
         define variable cInclude  as character no-undo.
         className  = NameService:ClassName(pcTable).
         className  = caps(substr(className,1,1)) + substr(className,2).
         return lc(substr(className,1,1)) + substr(className,2). 
    end method.
    
    method public character Generate(pcDb    as character,
                                     pcTable as character): 
                                         
        return Generate(pcDb, pcTable, GetIncludeName(pcTable)).
    end method.
    
    method public character Generate(pcDb    as character,
                                     pcTable as character,
                                     pcInclude as character): 
    
        define variable Service         as IDataAdminService no-undo.
        define variable DbTable         as ITable no-undo.
        define variable Indexes         as IIndexSet no-undo.
        define variable ResourceModel   as ResourceModel no-undo.
        define variable className       as character no-undo.
        define variable entityname      as character no-undo.
        define variable tempTablename   as character no-undo.
        define variable beforeTablename as character no-undo. 
        define variable oDataFields     as ISet no-undo.
        define variable oGenerator      as CodeGenerator no-undo.
        define variable info            as character no-undo.
        define variable cIndexStringsDummmy as character extent no-undo.
        
        Service   = new DataAdminService(pcDb).
        
        DbTable   = Service:GetTable(pcTable).
        if not valid-object(DbTable) then 
            undo, throw new IllegalArgumentError(subst("Table &1 was not found in database &2",pctable,pcDb)).   
        
        ResourceModel = new ResourceModel().
        oDataFields = ResourceModel:GetDataFields(dbTable).
        ResourceModel:AddPublicNameToFields(oDataFields, NameService).
             
        assign
            Indexes          = DbTable:indexes
            className        = NameService:ClassName(pcTable)
            classname        = caps(substr(className,1,1)) + substr(className,2)
         
            entityName       = NameService:PluralizeName(lc(substr(className,1,1)) + substr(className,2)) 
            tempTableName   = "tt" + className
            beforeTableName = "bi" + className
            info = subst("Mapped to database &1 table &2",pcDb,pcTable) 
            .
        CodeGenerator:ApplicationPath = OutputDir.   
        CodeGenerator:GenerateDefinition(pcInclude, TempTableName, entityName, BeforeTablename, oDataFields, Indexes, cIndexStringsDummmy, info).
        pcInclude = CodeGenerator:GetRelativeIncludeName(pcInclude).
        return pcInclude.
    end.
    
    constructor public DefinitionGenerator (pOutputdir as character, pNameService as INameService, pGenerator as CodeGenerator):
        super ().
        OutputDir   = pOutputdir.
        NameService = pNameService.
        CodeGenerator = pGenerator.
    end constructor.
    
end class.
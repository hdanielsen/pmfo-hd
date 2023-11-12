
/*------------------------------------------------------------------------
    File        : INameService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 27 19:19:16 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.Common.INameService:  
    // public name for table from db table
    method public character EntityName(pcName as char).
    
    /** public name for table with option to say yes generate from data that allows any character including spaces (i.e Code table key) 
    method public character PublicEntityName(pcName as char,plUseSpaceNameLogic as logical).
    ***/
    method public character PluralizeName(pcName as char).
         
    // public name for field
    method public character PublicFieldName(pcName as char).
    
    // class name (basis for datasource and business entity)
    method public character ClassName(pcName as char).
    
    // class name (basis for datasource and business entity) with option to say yes generate from data that allows any character including spaces (i.e Code table key) 
    method public character ClassName(pcName as char, plSpacedFields as logical).
    
    // returns true for tables notin use  
    method public logical isTableNotInUse(pcTableName as char).
    
end interface.
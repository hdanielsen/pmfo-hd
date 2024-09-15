 
 /*------------------------------------------------------------------------
    File        : FieldModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Feb 28 12:45:19 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Progress.Lang.Object from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.

class Pmfo.Tools.Gui.Model.FieldModel: 
    define property name            as char     get. set.
    define property serializeName   as char     get. set.
    define property serializeHidden as logical  get. set.
    define property sourceDb        as char     get. set.    
    define property sourceTable     as char     get. set.    
    define property sourceName      as char     get. set.    
    define property dataType        as char     get. set.
    define property inSDO           as logical  get. set.
    define property IsCalc          as logical  get. set.
    define property isJoin          as logical  get. set.
    define property IsExcluded      as logical  get. set.
    define property calculation     as longchar get. set. 
    
    method public override logical Equals(pObject as Object): 
        if type-of(pObject,FieldModel) then 
             return  cast(pObject,FieldModel):name = name 
                 and cast(pObject,FieldModel):sourceTable = sourceTable
                 and (sourceDb = ""
                      or cast(pObject,FieldModel):sourceDb = ""
                      or cast(pObject,FieldModel):sourceDb = sourceDb
                      )
                   .   
        return false.                       
    end method.
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public FieldModel ():
        super().
    end constructor.
    
    constructor public FieldModel (pcTableName as character, pcName as character):
        super ().
        name = pcName.
        sourcename = pcName.
        sourceTable = pcTableName.
        
    end constructor.

end class.
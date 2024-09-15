 
 /*------------------------------------------------------------------------
    File        : DlpModel
    Purpose     : 
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : Sat Feb 23 10:06:30 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Pmfo.Tools.Gui.Model.AppBuilderModel from propath.

class Pmfo.Tools.Gui.Model.DlpModel inherits AppBuilderModel  : 
    define override property ProcedureType as char no-undo init "DataLogicProcedure" get. 
    define property DataLogicTable as char no-undo get. set.
    define variable PathName as character  no-undo.
    constructor public DlpModel (  ):
        super().
    end constructor.
    
    // for dynamics  @TODO make general behavior
    constructor public DlpModel (pcPath as character  ):
        this-object().
        PathName = pcPath.
    end constructor.
    
    method public override logical Parse(pcfilename as char  ):
        define variable lok as logical no-undo.
        lok = super:Parse(pcFilename).
        if Pathname > "" then 
            this-object:name = subst("&1/&2",Pathname,this-object:name). 
        return lok.    
    end method.
         
    method override protected void checkCodeLine(pcSection as char,pcline as char):
        if pcSection = "Definitions" then 
        do:
           checkAndAssignDataLogicTable(pcline). 
        end.    
    end method.
    
    method protected void checkAndAssignDataLogicTable(pcline as char):
        if pcLine matches "&GLOB*-LOGIC-TABLE *" then 
        do:
            pcLine = trim(pcLine).
            DataLogicTable = entry(num-entries(pcLine," "),pcLine," ").
        end.
    end.    
    
    method override protected logical ReadLine( pcLine as char ):
        define variable lOk    as logical no-undo.
        lok = super:ReadLine(pcLine).
         
        if lok then do:
            checkAndAssignDataLogicTable(pcline). 
        end.    
    end.
    
end class.
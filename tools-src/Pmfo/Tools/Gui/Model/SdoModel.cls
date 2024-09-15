 
 /*------------------------------------------------------------------------
    File        : SdoModel
    Purpose     : 
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : Fri Feb 22 10:06:30 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.Model.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using Pmfo.Tools.Gui.Model.AppBuilderModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
using Pmfo.Tools.Gui.Model.AbstractSdoModel from propath.
 

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.SdoModel inherits AbstractSdoModel  : 
    constructor public SdoModel (  ):
        super().
    end constructor.
    
    method override public logical Parse(pcfilename as char  ):
        define variable lOk as logical no-undo.
        lOk = super:Parse(pcfilename).
        return lok.
        finally:
            if not lok then 
                DataLogicProcedure = ?.
        end.    
    end method.    
    
    method override protected void CheckCodeLine(pcSection as char,pcline as char):
        
        if pcSection = "Definitions" then
        do: 
            checkAndAssignDlp(pcLine).
        end.
        else if pcSection = "Settings" then 
        do:    
           if trim(pcLine) = "Temp-Tables and Buffers:" then 
               ReadSettings = true.
           else if ReadSettings then do:
               if trim(pcLine) = "END-TABLES." then
                  ReadSettings = false.
               else do:
                   pcline = trim(pcLine).
                   if pcLine begins "TABLE: " then do:
                       if(entry(3,pcLine," ")) = "B" then 
                       do:
                           create ttBuffer.
                           assign 
                               ttBuffer.name      = entry(2,pcLine," ").
                               ttBuffer.db        = entry(6,pcLine," ").
                               ttBuffer.tableName = entry(7,pcLine," ").
                       end. 
                       // if there is a Temo-table LIKE  for the table then find the real table 
                       else if(entry(3,pcLine," ")) = "T" then 
                       do:
                           // the LIKE table  
                           if this-object:Table = entry(2,pcLine," ") 
                           // and entry(6,pcLine," ") = "tempdb" (not sure if we should only do this for tempdb) 
                           and this-object:Table <> entry(7,pcLine," ") then
                           do:
                               this-object:Table = entry(7,pcLine," ").
                           end.    
                       end.
                   end.         
               end.       
           end.        
        end.
    end.    
    
    method protected void CheckAndAssignDlp(pcline as char):
        if pcLine matches "&glob* DATA-LOGIC-PROCEDURE *" then 
        do:
            pcLine = trim(pcLine).
            DataLogicProcedure = GetFullDlpName(entry(num-entries(pcLine," "),pcLine," ")).
        end. 
    end.    
    
      
    // return the assignlist converted for BE 
    method public character GetAssignListNoRowObject():
        return left-trim(replace("," + AssignList,",RowObject.",","),",").  
    end.     
    
   
    
    method override protected logical ReadLine( pcLine as char ):
        define variable lOk    as logical no-undo.
        define variable cTable as character no-undo.
        lok = super:ReadLine(pcLine).
       
        if lok then do:
            checkAndAssignDlp(pcLine).
            if pcLine begins "&Scoped-define QUERY-NAME" then 
            do:
                QueryName = entry(3,pcLine," ").
            end.
            else if QueryName > "" and pcLine begins "&Scoped-define FIRST-TABLE-IN-QUERY-" + Queryname then
            do:
                ctable = entry(3,pcLine," ").
                if num-entries(cTable,".") = 2 then
                    assign
                        this-object:Table = entry(2,cTable,".")
                        this-object:DataBase = entry(1,cTable,".")
                        . 
                else
                    this-object:Table  = cTable.  
            end.  
            else if QueryName > "" and pcLine begins "&Scoped-define QUERY-STRING-" + Queryname then
            do:
                pcLine = substr(pcline,length("&Scoped-define QUERY-STRING-" + Queryname) + 1).
                BaseQuery = ReadMultiLineDefinition(pcline).  
            end. 
             // convert tables-in-query-{&query-name} into comma separated list 
            else if pcLine begins "&Scoped-define INTERNAL-TABLES" then 
            do:
                pcLine = trim(substr(pcline,length("&Scoped-define INTERNAL-TABLES") + 1)).
                pcLine = ReadMultiLineDefinition(pcline).
                QueryTables = replace(pcLine," ",",").
            end.
            // convert assignlist nto comma separated list 
            else if pcLine begins "&Scoped-Define ASSIGN-LIST" then 
            do:
                pcLine = trim(substr(pcline,length("&Scoped-Define ASSIGN-LIST") + 1)).
                pcLine = ReadMultiLineDefinition(pcline).
                pcLine = replace(pcLine," = ",",").
                pcLine = replace(pcLine," ",",").
                do while index(pcLine,",,") > 0:
                    pcLine = replace(pcLine,",,",",").
                end.    
                AssignList = pcLine.
            end.
            else if pcLine begins "&Scoped-Define DATA-FIELDS" then do:
                pcLine = trim(substr(pcline,length("&Scoped-Define DATA-FIELDS") + 1)).
                pcLine = ReadMultiLineDefinition(pcline).
                DataFields = replace(pcLine," ",",").
            end.    
            else if pcLine begins "&Scoped-Define ENABLED-FIELDS" then do:
                pcLine = trim(substr(pcline,length("&Scoped-Define ENABLED") + 1)).
                pcLine = ReadMultiLineDefinition(pcline).
                EnabledFields = replace(pcLine," ",",").
            end.    
            else if pcLine begins "&ANALYZE-SUSPEND _QUERY-BLOCK QUERY " + QueryName then do:
                this-object:Fields = loadFields().
            end.
        end.
        return lok.
    end method.
    
    method protected ISet LoadFields ():
        define variable cFieldName as character no-undo.
        define variable cDbField   as character no-undo.
        define variable cDataType  as character no-undo.
        define variable cLine      as character extent 10 no-undo.
        define variable iline      as integer no-undo.
        define variable oField     as FieldModel no-undo.
        define variable ofields    as ISet no-undo.
        define variable ctest      as character no-undo.
        define variable cDatabases as character extent no-undo.
        define variable iTblNum    as integer no-undo.
        oFields = new Set().
        repeat:
            
            import stream-handle StreamHandle cline no-error.
            
            if cline[1] begins "_Design-Parent" then leave.
            if cLine[1] = "&ANALYZE-RESUME" then leave.
            if iLine = 0 then 
            do: 
               if cLine[1] begins "_FldNameList" then iLine = 1.
               else next.
            end.
            else do:
                if iLine = 1 then iLine = 2.
                else if iLine = 2 then iLine = 1.
            end.
            if iLine = 1 then
            do:
                oField = new FieldModel().
                oField:inSDO = true.
                ofields:Add(oField).    
            end. 
            if iLine = 1 then do:
                if cLine[3] = "_<CALC>" then 
                do:
                   oField:IsCalc = true.
                end. 
                else do  :
                    assign
                        oField:sourceDb    = entry(1,cLine[3],".")
                        oField:sourceTable = entry(2,cLine[3],".")
                        oField:sourceName  = entry(3,cLine[3],".")
                        oField:isJoin      = oField:sourceTable <> this-object:Table
                       .
                    if oField:sourceTable = this-object:Table and this-object:Database = "" then
                        this-object:Database = oField:SourceDb.
                    else do:
                        if extent(cDatabases)  = ? then 
                            extent(cdatabases) = num-entries(QueryTables).
                        iTblNum = lookup(ofield:sourceTable,QueryTables).
                        if iTblNum > 0 then
                             cDatabases[iTblNum] = ofield:sourceDb.
                            
                    end.                   
                end.      
            end.
            else do:          
                assign      
                    oField:name  = cLine[2]
                    oField:dataType = cLine[5].
                if oField:isCalc then
                    oField:calculation = cLine[1].
            end.   
        end.
        
        if oFields:Size = 0 then 
        do:
            delete object oFields.
            return ?.
        end.
    // makes no sense after CheckCodeLine was fixed to support TABLE type "T"
    // TODO there is some improvements that can be done for multi-tab;le queries  
    // to find the rela table - possibly spltting the query wiht QueryUtil and find the last each     
        /* if the first table in query ihave no fields we did not find database  */
/*        if this-object:DataBase = "" then                        */
/*        do iTblNum = 1 to extent(cDatabases) :                   */
/*            if cDatabases[iTblNum] > "" then                     */
/*            do:                                                  */
/*               this-object:Table    = entry(iTblNum,QueryTables).*/
/*               this-object:DataBase = cDatabases[iTblNum].       */
/*               leave.                                            */
/*            end.                                                 */
/*        end.                                                     */
        return oFields.
    end method.     
    
    /* The current assumption is that th DLP is in the same directory.
       This allows this to work when the sdo is not in propath 
       TODO: Consider changing this to require propath and use search 
            
    */ 
  
    method protected character GetFullDlpName(pdlpName as char):
        define variable i as integer no-undo.
        define variable istart as integer no-undo.
        define variable cin as character no-undo.
        cin = pdlpname.
        if pdlpname = ".p" then 
            return "".
        pdlpname = replace(pdlpname,"/","~\").
        iStart = num-entries(Name,"~\") - num-entries(pdlpName,"~\") .
        do i = iStart to 1 by -1:
            pdlpname = entry(i,Name,"~\") + "~\" + pdlpname.
        end.
      
        return pdlpname.
    end method.    
   
    
end class.
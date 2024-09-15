 
 /*------------------------------------------------------------------------
    File        : AppBuilderModel
    Purpose     : 
    Syntax      : 
    Description :  
    Author(s)   : hdaniels
    Created     : Sat Feb 23
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.Model.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using Pmfo.Tools.Gui.Model.ProcedureModel from propath.
using Pmfo.Tools.Gui.Model.FunctionModel from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.AppBuilderModel abstract  : 
    define public abstract property ProcedureType as char no-undo get. 
    define public property Name as character no-undo get. protected set.
    define public property Procedures as ISet no-undo 
        get():
            if not valid-object(Procedures) then 
               Procedures = new Set().
            return Procedures.   
        end. 
        set.     
        
    define public property Functions as ISet no-undo 
        get():
            if not valid-object(Functions) then 
               Functions = new Set().
            return Functions.   
        end. 
        set. 
            
    define public property Definitions as longchar no-undo get. protected set.
    define public property MainBlock as longchar no-undo get. protected set.
   
    define public property Table as character no-undo get. protected set.
    define public property QueryTables as character no-undo get. protected set.
    define protected property StreamHandle as handle no-undo get. set.     
    
    define stream filestream.
          
    constructor public AppBuilderModel (  ):
        StreamHandle = stream filestream:handle.
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method protected logical ReadLine( pcLine as char ):
        
        if pcLine begins "&Scoped-define PROCEDURE-TYPE" then 
        do:
            if entry(3,pcLine," ") <> ProcedureType then do:
                return false. 
            end.    
        end.    
/*                if lFound and pcLine begins "&Scoped-define QUERY-NAME" then                                */
/*                do:                                                                                         */
/*                    QueryName = entry(3,pcLine," ").                                                        */
/*                end.                                                                                        */
/*                if QueryName > "" and pcLine begins "&Scoped-define FIRST-TABLE-IN-QUERY-" + cQueryname then*/
/*                do:                                                                                         */
/*                    this-object:Table  = entry(3,cLine," ") .                                               */
/*                end.                                                                                        */
        if pcLine begins "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS" then 
            Definitions = ParseDefinitions().  
        else if pcLine begins "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK" then  
            MainBlock = ParseCode().
        else if pcLine begins "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE" then  
            Procedures:Add(ParseProcedure()). 
        else if pcLine begins "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION" 
             and ((pcLine begins "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD") = false) then  
            Functions:Add(ParseFunction()).
        else if pcLine = "&ANALYZE-SUSPEND _PROCEDURE-SETTINGS" then do:
            ParseSettings(). 
        end.         
        return true. 
    end method.

    method public logical Parse(pcfilename as char  ):
        define variable cline      as character no-undo.
        define variable lfirstline as logical no-undo.
        define variable lOk        as logical no-undo.
        define variable cQueryName as character no-undo.
        
        input stream filestream from value(pcfilename).
        repeat:  
            import stream filestream unformatted cline.
            // check if this is appbuilder code 
            if not lfirstline then do:
               if not cLine begins "&ANALYZE-SUSPEND _VERSION-NUMBER" then 
                   return false. 
               lfirstline = true.      
               Name = pcFilename.    
            end.       
            else do:
                lOk = ReadLine(cLine).
                
                if not lok then 
                    return false.
            end.    
        end.
        return true.
        finally:
            if not lOk then do: 
                Definitions = ?.
                Name = "".
                //DataLogicProcedure = "".
            end.     
           
            input stream filestream close.
        end.
    end method.    
    
    /** read multi-line preprocessor definition that is appended with ~ */
    method protected character ReadMultiLineDefinition(pcFirstLine as char):
        define variable cLine     as character no-undo.
        define variable cAllLines as character no-undo.
        cLine = pcFirstLine.
        if cLine > "" then
        repeat:
            if substr(cLine,length(cLine),1) <> "~~" then do:
                cAllLines = cAllLines + cline.
                leave. 
            end.    
            cAllLines = cAllLines + substr(cLine,1,length(cLine) - 1).
            import stream filestream unformatted cline.
        end.
        // TODO remove spaces - careful as they could be inside quotes - use maskquotes from adm2
        return trim(cAllLines).    
    end method.
    
    method protected longchar ParseDefinitions():
        return ParseCode(true,"Definitions").
    end method.  
    
    method protected longchar ParseSettings():
        return ParseCode(true,"Settings").
    end method.    
    
    method protected ProcedureModel ParseProcedure():
        define var oproc as ProcedureModel no-undo.
        define variable cLine as character no-undo.
        import stream filestream unformatted cline.
          // TODO remove double spaces
        oProc = new ProcedureModel(entry(2,cLine," ")).
        oProc:code = ParseCode().
        return oProc.  
    end.    
    
    method protected FunctionModel ParseFunction():
        define var ofunc as FunctionModel no-undo.
        define variable cLine as character no-undo.
        import stream filestream unformatted cline.
        // TODO remove double spaces
        ofunc = new FunctionModel(entry(2,cLine," ")).
        oFunc:DataType = entry(4,cLine," ").
        ofunc:code = ParseCode().
        return ofunc.  
    end.    
    
    method protected longchar ParseCode():
        return ParseCode(false,"").  
    end method.    
    
    /* override to allow sub class to check for specific data i 
       currently only used for definitions
     */
    method protected void checkCodeLine(pcSection as char,pcline as char):
    end.    
    
    method protected longchar ParseCode(plcheckline as log, pcsection as char):
         define variable cLong       as longchar no-undo.
         define variable cSearchLong as character no-undo.
         define variable cLine as character no-undo.
         define variable i as integer no-undo.
       
         repeat:  
            i = i + 1. 
            import stream filestream unformatted cline.
            
            if plcheckLine then do:
                checkCodeLine(pcSection, cLine).
            end.      
            /* skip the comment it is editable and can have been moved */
            if trim(cline) = "/* _UIB-CODE-BLOCK-END */" then next.   
            if cLine begins "&ANALYZE-RESUME" then leave.
            cLong = cLong + ( if i > 1 then "~n" else "") + cline.
         end.
/*                                                                    */
/*         define variable iFound       as integer no-undo.           */
/*         define variable iParenthesis as integer no-undo.           */
/*         define variable cParenthesis as character no-undo.         */
/*         define variable iCharloop as integer no-undo.              */
/*         define variable cChar as character no-undo.                */
/*         define variable iChARHARsTARTIcHrStart as integer no-undo. */
/*         iFound = 1.                                                */
/*                                                                    */
/*         do while true:                                             */
/*                                                                    */
/*             iFound = index(cLong,"dynamic-function",iFound).       */
/*/*             if cSearchLong = ? or cSearchLong = "" then        */*/
/*/*                 cSearchLong = StringUtil:MaskQuotes(cLong,"@").*/*/
/*                                                                    */
/*             if iFound > 0 then                                     */
/*             do:                                                    */
/*                 message                                            */
/*                  name skip                                         */
/*                  string(substr(cLong,iFound))                      */
/*                 view-as alert-box.                                 */
/*                 iFound = iFound + 20.                              */
/*             end.                                                   */
/*             else leave.                                            */
/*                                                                    */
/*         end.                                                       */
/*                                                                    */
         return cLong.
    end method.    
    
end class.
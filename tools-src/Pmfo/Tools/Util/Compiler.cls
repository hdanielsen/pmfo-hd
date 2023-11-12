 
 /*------------------------------------------------------------------------
    File        : Compiler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 05 08:41:25 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.FileIO from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.
using OpenEdge.Core.StringConstant from propath.

block-level on error undo, throw.
         
class Pmfo.Tools.Util.Compiler: 
    {Pmfo/Util/filedata.i}   
    define temp-table ttError no-undo
         field sequence   as integer
         field classType as character
         field fileoffset as integer 
         field errorRow as integer 
         field msg as character format "x(70)"  
         index ids as primary unique sequence. 
   
    define public property MaxErrors as integer no-undo init 10 get. set.
    define protected variable MaxReached as logical no-undo.
    define variable Months as character extent 12 init ['January','February','March','April','May','June','July','August','September','October','November','December'] no-undo.       
    define protected property FileIo as FileIO no-undo
         get():
           //  if not valid-object(FileIO) then 
             //   FileIo  = new FileIO().
             return new FileIO().   
         end.
         set.    
    
    method character GetStringDate(pdt as datetime):
       return subst("&1 &2 &3 &4",day(pdt),months[month(pdt)],year(pdt),string(time,"hh:mm:ss")).  
    end.     
    
    method public void CompileAll (pcdir as character, pMonitor as IMonitor):
        Compile(pcdir,true,"",pMonitor). 
    end method.
    
    
    method public void CompileAll (pcdir as character):
        Compile(pcdir,true,"", ?). 
    end method.
     
    method public void Compile (pcdir as character, plRecursive as logical, pcSkipdir as character,pMonitor as IMonitor):
        define variable i as integer no-undo.
        define variable iCount as integer no-undo.
        empty temp-table fileData.
        empty temp-table ttError.
        
        MaxReached = false.
        fileio:getFiles(pcdir, plRecursive, pcSkipDir, output table fileData).
        for each fileData where filedata.fileExt = "cls" or filedata.fileExt = "p" or filedata.fileExt = "w":
            compile value(filedata.fullpath) save no-error.
            if compiler:error then 
            do:
                i = i + 1.
                create ttError.
                assign 
                    ttError.sequence = i
                    tterror.fileoffset = compiler:file-offset
                    tterror.classtype = compiler:class-type
                    tterror.errorrow = compiler:error-row
                    ttError.msg      = subst("&1: &2",filedata.fullName,compiler:get-message(1))
                    .  
                if i = maxErrors then 
                do:
                    MaxReached = true.
                    return.
                end.                 
            end.      
/*            else                                                                   */
/*               display fileData.fullPath format "x(70)" //  fullName format "x(20)"*/
/*         
   .                                                                      */
        end.
    end method.
     
    method public void Refresh (pcDir as character):
        define variable i as integer no-undo.
        define variable iCount as integer no-undo.
        empty temp-table fileData.
        empty temp-table ttError.
        current-window:width = 300.
        output to "refresh.ps1".
        MaxReached = false.
        empty temp-table fileData.
        fileio:getFiles(pcdir, true,"", output table fileData).
       // put unformatted "powershell" skip.
        //os-command powershell.
        define variable dtdatetime as datetime no-undo.
        dtdatetime = now.
        for each fileData where filedata.fileExt = "cls" // or filedata.fileExt = "p" or filedata.fileExt = "w"
        
        //  with width 300
            :
     //       disp fileData.fileTime.
          put unformatted subst('(Get-Item "&1").LastWriteTime=("&2")',filedata.fullPath,GetStringDate(dtdatetime)) skip .
             //)powershell subst('(Get-Item "&1").LastWriteTime=("&2")',filedata.fullPath,GetStringDate(dtdatetime)).  
             // '"' + filedata.fullName + '").LastWriteTime=(=("' 
            // + GetStringDate(dtdatetime) skip.
             
        end.
        output close.
        os-command silent powershell -executionpolicy bypass -File refresh.ps1.
           
    end method.
    
    method public void ShowTime (pcDir as character):
        define variable i as integer no-undo.
        define variable iCount as integer no-undo.
        empty temp-table fileData.
        empty temp-table ttError.
        current-window:width = 300.
        MaxReached = false.
        empty temp-table fileData.
        fileio:getFiles(pcdir, true,"", output table fileData).
       // put unformatted "powershell" skip.
        //os-command powershell.
        define variable dtdatetime as datetime no-undo.
        dtdatetime = now.
        for each fileData where filedata.fileExt = "cls" // or filedata.fileExt = "p" or filedata.fileExt = "w"
            :
            disp filedata.fullName format "x(32)" fileData.fileTime.
        end.
           
    end method.
    
    method public character CompileLastCrash(): 
       file-info:file-name = ".". 
       return CompileLastCrash(file-info:full-pathname).
    end method.
    
    
    method public character CompileLastCrash(pcDir as character): 
        define variable cProtracefile as character no-undo.
        define variable cLine as character no-undo.
        define variable lnext as logical no-undo.
        define variable cProg as character no-undo.
        empty temp-table fileData.
        // current-window:width = 300. 
        
        fileio:getFiles(pcdir, false, "", output table fileData).
        for each fileData // eith width 300 
            by filedata.fileTime descending:
            if filedata.fullName begins "protrace." then
            do: 
                cProtracefile = filedata.fullPath.
                leave.
            end.         
        end.
        input from value(cProtracefile).
        repeat: 
            import unformatted cline .
            if lnext and cline > "" then 
                 leave.
            if cline = "** ABL Stack Trace **" then 
                lnext = true.
        end.    
        if lnext then 
        do:
            cProg = entry(2,cline,"").
            cProg = replace(cProg,".","/").
            compile value(subst("&1.cls",cProg)) save.
            return cProg.   
        end.     
                       
    end method.
    
    method public void ThrowErrors():
        define variable oError as AppError no-undo.
        for each ttError:
            if not valid-object(oError) then 
            do:
                if Maxreached then
                do: 
                     oError = new ApplicationError(subst("Compilation stopped. MaxEerrors set to &1",MaxErrors)).
                     oError:AddMessage(ttError.msg, ?).
                end.
                else     
                     oError = new ApplicationError(ttError.msg).
            end.    
            else 
                oError:AddMessage(ttError.msg, ?).
        end.
        if valid-object(oError) then 
            undo, throw oError.
    end method.    
        
end class.
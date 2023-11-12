 
 /*------------------------------------------------------------------------
    File        : ErrorTracker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 30 10:51:52 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.Array from propath.
using OpenEdge.DataAdmin.Error.IllegalArgumentError from propath.
using Progress.Reflect.Property from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Tools.AppBuilder.ErrorTrackerEnum from propath.
using OpenEdge.Core.Assert from propath.

block-level on error undo, throw.

class Pmfo.Tools.AppBuilder.ErrorTracker: 
   
    define temp-table ttError 
        field Prop  as Object 
        field Msg   as char
        field id1   as char 
        field id2   as char
        field id3   as char
        field id4   as char
        index errornum as primary unique Prop id1 id2.
        
    constructor public ErrorTracker():
        super().
    end constructor.
    
    method public void AddError(pErrorEnum as ErrorTrackerEnum):
        AddError(pErrorEnum,"").
    end.
    
    method public void AddError(pErrorEnum as ErrorTrackerEnum,pId as char):
        AddError(pErrorEnum,Array:ToArray(pId)). 
    end.
            
    method public void AddError(pErrorEnum as ErrorTrackerEnum,pIds as char extent):
        
        if FindError(pErrorEnum, pIds) then 
            return. 
        
        create ttError.
        
        assign 
            ttError.Prop = pErrorEnum.
            
        case extent(pids):
            when 1 then 
                assign
                    ttError.Msg = subst(GetMessage(pErrorEnum),pIds[1])
                    ttError.Id1  = pIds[1]
                    .
            when 2 then
                assign
                    ttError.Msg = subst(GetMessage(pErrorEnum),pIds[1],Pids[2])
                    ttError.Id1  = pIds[1]
                    ttError.Id2  = pIds[2]
                    .
            when 3 then
                assign
                    ttError.Msg = subst(GetMessage(pErrorEnum),pIds[1],pIds[2],pIds[3])
                    ttError.Id1  = pIds[1]
                    ttError.Id2  = pIds[2]
                    ttError.Id3  = pIds[3]
                    .
            when 4 then
                assign
                    ttError.Msg = subst(GetMessage(pErrorEnum),pIds[1],pIds[2],pIds[3], pids[4])
                    ttError.Id1  = pIds[1]
                    ttError.Id2  = pIds[2]
                    ttError.Id3  = pIds[3]
                    ttError.Id4  = pIds[4]
                    .
            otherwise do: // we can support 9 if necessary
                delete ttError.
                undo, throw new IllegalArgumentError("Max 4 extents allowed in AddError second parameter").
            end.
        end.
    end.           
    
    method public void RemoveError(pErrorEnum as ErrorTrackerEnum,pIds as char extent):
        release ttError.
        FindError(pErrorEnum, pIds). 
        if avail ttError then 
            delete ttError.
        catch e as Progress.Lang.Error :
            undo, throw new NotFoundError(subst("Error &1",pErrorEnum:ToString())).
        end catch.
    end.           
    
    method public logical FindError(pErrorEnum as ErrorTrackerEnum,pIds as char extent):
        case extent(pids):
            when 1 then
                find ttError where ttError.Prop = pErrorEnum
                               and ttError.id1 = pIds[1] 
                               no-error.
            when 2  then    
                find ttError where ttError.Prop = pErrorEnum
                               and ttError.id1 = pIds[1] 
                               and ttError.id2 = pIds[2] 
                               no-error.
            when 3 then    
                find ttError where ttError.Prop = pErrorEnum
                               and ttError.id1 = pIds[1] 
                               and ttError.id2 = pIds[2] 
                               and ttError.id3 = pIds[3] 
                               no-error.
                               
            when 4 then    
                find ttError where ttError.Prop = pErrorEnum 
                               and ttError.id1 = pIds[1] 
                               and ttError.id2 = pIds[2] 
                               and ttError.id3 = pIds[3] 
                               and ttError.id4 = pIds[4] 
                               no-error.
            otherwise // we can support 9 if necessary
                undo, throw new IllegalArgumentError("Max 4 extents allowed in FindError second parameter").
        end.
        return avail ttError.
    end.           
    
    method public logical HasMessage():
        return can-find(first ttError).
    end method.
    
    method public longchar GetMessages(pSkip as int):
        define variable cMsg as longchar no-undo.
        for each ttError:
            cMsg = cMsg + tterror.Msg + fill(StringConstant:LF,pSkip).
        end.
        return right-trim(cMsg,StringConstant:LF).
    end.
    
    method protected character GetMessage(pErrorEnum as ErrorTrackerEnum):
        define variable props as Property extent no-undo.
        define variable i as integer no-undo.
        Assert:NotNull(pErrorENum,"ErrorTrackerEnum passed to GetMessage()").
        case pErrorENum:
            when ErrorTrackerEnum:ISAddOrCopyReplace then
                return "Method &1 has one or more 'isAdd()' and/or 'isCopy()' function calls that must be replaced by 'row-state(<temp-table-name>) = row-created' manually" .
            when ErrorTrackerEnum:AddWarningReplace then
                return "Method &1 has one or more 'RowObject.rowValMsgs' assignments that must be replaced by AddWarning manually" .
            when ErrorTrackerEnum:AddMessageReplace then
                return "Method &1 has one or more run 'AddMessage' statements that must be replaced by BufferError manually" .
            when ErrorTrackerEnum:HasDynamicFunction then
                return "Method &1 has one or more dynamic-function calls that must be removed manually" .
            when ErrorTrackerEnum:TooLongTempTableName then
                return "Temp Table name '&1' is too long. Max length is 32 characters" .
            otherwise    
                return subst("Message for error '&1' is not defined in ErrorTracker",pErrorENum:ToString()) + ", but something is wrong with &1 &2 &3 &4".
        end case.        
    end method.
    
end class.
 
 /*------------------------------------------------------------------------
    File        : UpdateDataRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 02 09:34:23 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.BusinessLogic.UpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using OpenEdge.BusinessLogic.NamedQuery from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.Action from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.
using Pmfo.Core.Common.GetDataMessages from propath.

class Pmfo.Core.BusinessLogic.UpdateDataRequest inherits UpdateDataRequest implements IUpdateDataRequest: 
    define public property RequestType         as RequestTypeEnum  no-undo get. set. 
    define public property MainRequestedBuffer as character        no-undo get. set. 
    define public property Confirmations       as character        no-undo get. protected set. 
    
    define public property Messages as IGetDataMessages  no-undo 
        get():
            if not valid-object(Messages) then 
                Messages = new GetDataMessages().
            return Messages.    
        end.    
        protected set.
        
    /* Optional comma separated list of tables that do not need or do not have
       an update target - read-only - yse Add- and RemoveNoTargetTable to edit */
    define public property NoTargetTables      as character no-undo get. protected set.
    
      /* Optional comma separated list of tables that need optimistic locking 
       if submitted with no changes
       read-only - yse Add- and Remove to edit */
    define public property CheckReadOnlyTables      as character no-undo get. protected set. 
    
    // mainly for actions with no Content reponse, but can be set manually 
    // when an updarte is called from an action with no content response and the 
    // action is not passed to the server or othjer cases where the data response is irrelevant  
    define public property NoRefresh           as logical no-undo get. set.
     
      /* Convenience read-only property to simplify checking for action */ 
    define public property Action              as IAction no-undo 
        get():
             if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
                 return cast(CustomRequest,IAction).
             else
                 return ?.    
        end.     
     
      /* Convenience read-only property to simplify checking for action in hooks*/ 
    define public property ActionName          as character no-undo 
        get():
             if valid-object(Action) then
                 return Action:name.
             else
                 return "".    
        end.     
    
    constructor public UpdateDataRequest (pBuffername as character ):
        this-object (pBuffername,RequestTypeEnum:SingleTopTable).
    end constructor.      
          
    constructor public UpdateDataRequest (pBuffername as char, pRequestType as RequestTypeEnum  ):
        super ().
        RequestType = pRequestType.
        MainRequestedBuffer = pBuffername.
    end constructor.
    
    constructor public UpdateDataRequest (pBuffername as char, pRequestType as RequestTypeEnum, pConfirmations as char ):
        this-object(pBufferName,pRequestType).
        Confirmations = pConfirmations.
    end constructor.
    
    method public void RemoveAction().
        if valid-object(Action) then 
        do:
            // Garbage Collection will get rid if the class
            CustomRequest = ?.
        end.
        else 
            undo, throw new IllegalOperationError("RemoveAction called with no valid action"). 
    end method.
    
    method public void ConvertActionToNamedQuery().
        // action name will be blank if no action, but invalid action will throw error as needed      
        ConvertActionToNamedQuery(ActionName).
    end method.
    
    method public void ConvertActionToNamedQuery(pcName as character).
        if valid-object(Action) then 
            CustomRequest = new NamedQuery(pcname).
        else 
            undo, throw new IllegalOperationError("ConvertActionToNamedQuery called with no valid action"). 
    end method.
    
    // add a table name to the no target table list
    method public logical AddNoTargetTable (pTablename as character):
        if lookup(pTablename,NoTargetTables) = 0 then
        do:
            NoTargetTables = NoTargetTables 
                           + (if NoTargetTables = "" then "" else ",")
                           + pTablename.
            return true.              
        end.            
        return false.      
    end method.
      
    // remove a table name from the no target table list
    method public logical RemoveNoTargetTable (pTablename as character):
        define variable iTable as integer no-undo.
        iTable = lookup(pTablename,NoTargetTables).
        if iTable > 0 then
        do:
            entry(iTable,NoTargetTables) = "". 
            NoTargetTables = trim(NoTargetTables,",").
            NoTargetTables = replace(NoTargetTables,",,",",").
            return true.              
        end.            
        return false.
    end method.    
    
    // add a table name to the CheckReadOnlyTables list
    method public logical AddCheckReadOnlyTable (pTablename as character).  
        if lookup(pTablename,CheckReadOnlyTables) = 0 then
        do:
            CheckReadOnlyTables = CheckReadOnlyTables 
                           + (if CheckReadOnlyTables = "" then "" else ",")
                           + pTablename.
            return true.              
        end.            
        return false.      
    end method.
    
    // remove a table name from the CheckReadOnlyTables list
    method public logical RemoveCheckReadOnlyTable (pTablename as character).  
        define variable iTable as integer no-undo.
        iTable = lookup(pTablename,CheckReadOnlyTables).
        if iTable > 0 then
        do:
            entry(iTable,CheckReadOnlyTables) = "". 
            NoTargetTables = trim(CheckReadOnlyTables,",").
            NoTargetTables = replace(CheckReadOnlyTables,",,",",").
            return true.              
        end.            
        return false.
    end method.
    
    /* Return true if the buffer or table is an action target */ 
    method public logical IsActionTarget(phHandle as handle):
         define variable cName as character no-undo.
         if not valid-handle(phHandle) then
            undo, throw new IllegalArgumentError("Handle is unknown"). 
         
         case phHandle:type:
             when "buffer" then 
             do:
                  if valid-handle(phHandle:after-buffer) then
                      cName = phHandle:after-buffer:table-handle:name.
                  else    
                      cName = phHandle:table-handle:name.
             end.
             when "table" then do:
                if valid-handle(phHandle:after-table) then
                    cName = phHandle:after-table:name.
                else    
                    cName = phHandle:name.
             end.
             otherwise undo, throw new IllegalArgumentError(subst("Invalid handle type &1",phHandle:type)). 
         end case.
         
         if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
         do: 
             return IsActionTarget(cName, cast(CustomRequest,IAction)).
         end.    
         return false.
    end method.    
    
    method protected logical IsActionTarget(pcTableName as character, pAction as IAction) :
        define variable oIterator as IIterator no-undo.
        
        if pAction:TargetTableName = "" then
            undo, throw new IllegalOperationError(subst("Call IsTargetAction before parameters have been set in Action '&1' ",pAction:name)).
        if pcTableName = pAction:TargetTableName then
            return true. 
             
/*        if pAction:HasChild then                                                  */
/*        do:                                                                       */
/*            oIterator = pAction:Children:Iterator().                              */
/*            do while oIterator:HasNext():                                         */
/*                if IsActionTarget(pcTableName,cast(oIterator:Next(),IAction)) then*/
/*                    return true.                                                  */
/*            end.                                                                  */
/*        end.                                                                      */
        return false. 
            
    end method.
     
    method public Progress.Reflect.Method GetClientMethod(pObject as Object, phDataset as handle,output pClientParameterList as ParameterList):
        if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
            return cast(CustomRequest,IAction):GetClientMethod(pObject,this-object, phDataset, output pClientParameterList).
        return ?.    
    end method.

    // The call to GetClientMethod may create a ClientParamterList with reference to this-object.
    // This is to be called after the method returned from GetClientMethod has been used to ensure GBC working as expected
    // later for the action and updateDataRequest      
    method public void RemoveClientMethod():
        if valid-object(Action) then
            Action:RemoveClientMethod().
    end method.
    
    method public character GetConfirmedQuestions  ( pcSourceEntityname as character):
        define variable cConfirmations      as character no-undo.
        define variable cConfirmation       as character no-undo.
        define variable cConfirmationEntity as character no-undo.
        define variable cConfirmationId     as character no-undo.
         
        define variable i as integer no-undo.
        
            // extract and convert Confirmations from client to ConfirmedQuestions 
        do i = 1 to num-entries(Confirmations):
            cConfirmation = entry(i,Confirmations).
            if cConfirmation > "" then
            do: 
                if num-entries(cConfirmation,"-") = 2 then 
                do:
                    cConfirmationEntity = entry(1,cConfirmation,"-").
                    if cConfirmationEntity = pcSourceEntityname then
                    do: 
                        cConfirmationId = entry(2,cConfirmation,"-").
                        cConfirmations   = cConfirmations
                                         + (if cConfirmations = "" then "" else ",")
                                         +  cConfirmationId.
                    
                    end.
               end.
               else 
                    undo, throw new IllegalOperationError(subst("UpdateRequest:Confirmations '&1' has invalid format",cConfirmations)).
           end.                    
       end.
       
       return cConfirmations.
   end.     
   
   
   
   method public override character ToString():
       return right-trim(subst("MainRequestedBuffer:&1 RequestType:&2&3&4&5&6",
                    MainRequestedBuffer,
                    RequestType:ToString(),
                    if valid-object (Action) then subst(" Action:&1",Action:ToString()) else "",
                    if NoTargetTables > "" then subst(" NoTargetTables:&1",NoTargetTables) else "",
                    if CheckReadOnlyTables > "" then subst(" CheckReadOnlyTables:&1",CheckReadOnlyTables) else "", 
                    if Confirmations > "" then subst(" Confirmations:&1",Confirmations) else "" 
                      
                   )). 
   end method.     
   
end class.
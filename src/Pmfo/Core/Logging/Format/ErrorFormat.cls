/* *************************************************************************************************************************
Copyright (c) 2016-2018 by Progress Software Corporation and/or one of its subsidiaries or affiliates. All rights reserved.
************************************************************************************************************************** */
/*------------------------------------------------------------------------
    File        : ErrorFormat
    Purpose     : Formats any errors
                  Replace OpenEdge. to fix bug in ExeciteFilter 
    Syntax      : 
    Description : 
    Author(s)   : pjudge
    Created     : 2016-11-21
    Notes       : * Written for all events
                    - the error messages and their number
                    - the return value (if an AppError)
                    - any errors contained in a InnerError property of type P.L.Error
                  * Written for DEBUG events
                    - the error type name
                  * Written for TRACE events
                    - the error's call stack
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using OpenEdge.Logging.Filter.ILoggerFilter.
using OpenEdge.Logging.LogEvent.
using OpenEdge.Logging.LogLevelEnum.
using Progress.Lang.AppError.
using Progress.Reflect.DataType.
using OpenEdge.Core.StringConstant.

class Pmfo.Core.Logging.Format.ErrorFormat implements ILoggerFilter:
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ErrorFormat (  ):
        super ().
    end constructor.

    /** Performs implementation-specific filtering for a logger type
        
        @param LogMessage The message to log. */
    method public void ExecuteFilter(input poEvent as LogEvent):
        define variable errLoop as integer no-undo.
        define variable errMax as integer no-undo.
        define variable ex as Progress.Lang.Error no-undo.
        define variable errProp as Progress.Reflect.Property no-undo.
        define variable errHeader as character no-undo.
         
        // only do error stuff
        assign ex        = poEvent:Error
               errHeader = 'Error(s) raised:'
               .
        do while valid-object(ex):
            // only log error type for DEBUG logging
            assign poEvent:Message:Message = poEvent:Message:Message
                                           + StringConstant:LF + StringConstant:TAB
                                           + errHeader.
            if  (     valid-object(poEvent:Logger) 
                  and poEvent:Logger:LogLevel ge LogLevelEnum:DEBUG )
               or poEvent:LogLevel ge LogLevelEnum:DEBUG then
                assign poEvent:Message:Message = poEvent:Message:Message + StringConstant:TAB + ex:GetClass():TypeName.
                
            assign errMax = ex:NumMessages.
            do errLoop = 1 to errMax:
                assign poEvent:Message:Message = poEvent:Message:Message
                                               + StringConstant:LF + StringConstant:TAB + StringConstant:TAB
                                               + ex:GetMessage(errLoop)
                                               + substitute(' (&1)':u, ex:GetMessageNum(errLoop) ).
            end.
            
            if     type-of(ex, AppError) 
               and cast(ex, AppError):ReturnValue ne '':u then
                assign poEvent:Message:Message = poEvent:Message:Message
                                               + StringConstant:LF + StringConstant:TAB
                                               + substitute('ReturnValue: &1' , cast(ex, AppError):ReturnValue).
            
            assign errMax = num-entries(ex:CallStack, StringConstant:LF).
            // Log error stacks for TRACE logging
            if (  (     valid-object(poEvent:Logger) 
                    and poEvent:Logger:LogLevel ge LogLevelEnum:TRACE )
                 or poEvent:LogLevel ge LogLevelEnum:TRACE 
                 or session:debug-alert                                )
               and errMax gt 0
            then
            do:
                assign poEvent:Message:Message = poEvent:Message:Message
                                               + StringConstant:LF + StringConstant:TAB
                                               + 'Error stack:'.
                do errLoop = 1 to errMax:
                    assign poEvent:Message:Message = poEvent:Message:Message
                                                   + StringConstant:LF + StringConstant:TAB + StringConstant:TAB
                                                   + entry(errLoop, ex:CallStack, StringConstant:LF).
                end.
            end.
            
            assign errProp = ex:GetClass():GetProperty('InnerError':u).
            if     valid-object(errProp) 
               and errProp:DataType eq DataType:Object 
               and Progress.Lang.Class:GetClass(errProp:DataTypeName):IsA(get-class(Progress.Lang.Error)) then
                assign errHeader = 'Caused by:'
                       ex        = errProp:Get(ex).
            else
                leave.
        end.
    end method.
        
end class.

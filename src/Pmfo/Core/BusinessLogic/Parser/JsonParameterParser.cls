 
 /*------------------------------------------------------------------------
    File        : JsonParameterParser
    Purpose     : Build a ParameterList from Json body 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Aug 28 08:01:17 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Parameter from propath.

class Pmfo.Core.BusinessLogic.Parser.JsonParameterParser : 
    
    define protected property Method   as Method no-undo get. set.
    // for error message 
    define protected property Resource as character no-undo get. set.
    
    define protected property PublicParameters as IParameter extent no-undo get. set.
    define protected property ParameterList as ParameterList no-undo get. set.
    
    define protected property ParameterNames as character init ? no-undo 
         get():
             define variable cDlm as character no-undo.
             define variable i as integer no-undo.
             define variable oParams as Parameter extent no-undo.
             if ParameterNames = ? then
             do:
                 ParameterNames = "". 
                 do i = 1 to extent(PublicParameters):
                    ParameterNames = ParameterNames
                                  + (if i = 1 then "" else ",")
                                  + PublicParameters[i]:name.                              
                 end.
                  
             end.
             return ParameterNames. 
         end. 
         set.
    
    constructor public JsonParameterParser ( pMethod as Method, pPublicParameters as IParameter extent, pcResource as character ):
        super ().
        Method = pMethod.
        Resource = pcResource.
        PublicParameters = pPublicParameters.
    end constructor.
    
    method public ParameterList GetParameterList(pJson as JsonObject):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        
        cNames = pJson:GetNames().
        
        ParameterList = new ParameterList(extent(cNames)).
        
        do i = 1 to extent(cNames):
             ProcessExpression(pJson,cNames[i]).
        end.    
        return ParameterList.
    end.   
    
    method protected void ProcessExpression( pJson as JsonObject, input pcName as character ):
        define variable iPosition as integer no-undo.
        define variable cLong as longchar no-undo.
        define variable oParams as Parameter extent no-undo.
       
        iPosition = lookup(pcName,ParameterNames).
       
        if iPosition = 0 then 
            undo, throw new BadRequestError(subst("'&1' is not a valid node for '&2'",pcName,Resource)).
             
        oParams = Method:GetParameters().
        
        case oParams[iPosition]:DataType:
            when Progress.Reflect.DataType:Character then
            do:
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pjson:GetCharacter(pcname)).
            end.
            when Progress.Reflect.DataType:Longchar then 
            do:
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pjson:GetLongchar(pcname)).
            end.
            
            when Progress.Reflect.DataType:Integer then
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pjson:GetInteger(pcname)).
            
            when Progress.Reflect.DataType:Int64 then
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pjson:GetInt64(pcname)).
            
            when Progress.Reflect.DataType:Decimal then
                 ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pJson:GetDecimal(pcName)).
                 
            when Progress.Reflect.DataType:Logical then
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pJson:GetLogical(pcName)).
            
            when Progress.Reflect.DataType:Date then
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pJson:GetDate(pcName)).
            
            when Progress.Reflect.DataType:Datetime then
                ParameterList:SetParameter(iPosition,oParams[iPosition]:DataType:ToString(),"input", pJson:GetDateTime(pcName)).
                
            when Progress.Reflect.DataType:Datetimetz then
                ParameterList:SetParameter(iPosition,"datetime-tz","input", pJson:GetDateTimeTz(pcName)).
                  
        end case.
        
               
        return.

    end method.

end class.
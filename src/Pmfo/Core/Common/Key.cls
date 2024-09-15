 
 /*------------------------------------------------------------------------
    File        : Key
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:35:07 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Support.ICharacterHolder from propath.
using Ccs.Common.Support.IDateHolder from propath.
using Ccs.Common.Support.IDateTimeHolder from propath.
using Ccs.Common.Support.IDateTimeTzHolder from propath.
using Ccs.Common.Support.ILongcharHolder from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.Common.Key implements IKey: 

    define public property FieldValues as IFieldValue extent no-undo 
    get.
    protected set. 
    
    // when used for non primary 
    define public property UseLabelForOne as logical no-undo 
    get.
    protected set. 
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
    constructor public Key (pValue as IFieldValue ):
        this-object(cast(Array:ToArray(pValue),IFieldValue)).
    end constructor.
    
    constructor public Key (pValue as IFieldValue, plUseLabelForOne as logical ):
        this-object(cast(Array:ToArray(pValue),IFieldValue)).
        UseLabelForone = plUseLabelForOne.
    end constructor.
    
    constructor public Key (pValues as IFieldValue extent):
        super ().
        FieldValues= pValues.     
    end constructor.
    
    method public JsonObject ToJson():
        define variable oJson as JsonObject no-undo.
        define variable i  as integer no-undo.
        oJson = new JsonObject().
        do i = 1 to extent(FieldValues):
            PrimitiveHolderUtil:AddToJson(FieldValues[i]:Name,FieldValues[i]:Value, oJson).  
        end.    
        return oJson.
    end method.
    
    method public JsonObject NameAndValueToJson():
        define variable oJson as JsonObject no-undo.
        define variable i  as integer no-undo.
         
        oJson = new JsonObject().
        do i = 1 to extent(FieldValues) on error undo, throw:
            PrimitiveHolderUtil:AddNameAndValueToJson(FieldValues[i]:Name,FieldValues[i]:Value, oJson).
             
        end.    
         
        return oJson.
        
    end method.
        
    method public override character ToString():
        return ToString("url"). 
    end method.     
    
    method public character ToString(pcQueryOption as character):
        define variable cKeys           as character no-undo.
        define variable iField          as integer no-undo.
        define variable cFld            as character no-undo.
        define variable hFld            as handle no-undo.
        define variable cValue          as character no-undo.
        define variable lHasPlus        as logical no-undo.
        define variable cRowKey         as character no-undo.
        define variable cExpressionSep  as character no-undo.
        define variable cPredicateSep   as character no-undo.
        define variable lUseLabel      as logical no-undo.
        
        if pcQueryOption = "UrlQuery" then
        do: 
           assign 
              cExpressionSep = " eq "
              cPredicateSep  = " and ". 
        end.
        else if pcQueryOption = "Url" then
        do: 
           assign 
              cExpressionSep = "="
              cPredicateSep  = ",". 
        end.
        else if pcQueryOption = "text" then
        do: 
           assign 
              cPredicateSep = " and ".
              lUseLabel = true. 
        end.    
                
        do iField = 1 to extent(FieldValues).
            if type-of(FieldValues[iField]:Value,IDateHolder) then
               cValue = quoter(iso-date(cast(FieldValues[IField]:Value,IDateHolder):Value),"'"). 
            else if type-of(FieldValues[iField]:Value,IDateTimeHolder) then
               cValue = quoter(iso-date(cast(FieldValues[IField]:Value,IDateTimeHolder):Value),"'"). 
            else if type-of(FieldValues[iField]:Value,IDateTimeTzHolder) then
                cValue = quoter(iso-date(cast(FieldValues[IField]:Value,IDateTimeTzHolder):Value),"'"). 
               //cValue = quoter(iso-date(FieldValues[IField]:ToString())).
            else 
            if type-of(FieldValues[iField]:Value,ICharacterHolder)  
            or type-of(FieldValues[iField]:Value,ILongcharHolder) then
            do: 
                if type-of(FieldValues[iField]:Value,String)  
                and cast(FieldValues[iField]:Value,String):Size gt 3000 then
                    undo, throw new NotImplementedError(subst("Key:ToString' of field with size &1. It seems unlikely that this is a true key.",cast(FieldValues[iField]:Value,String):Size)).  
                
                assign
                    cValue = FieldValues[iField]:Value:ToString()
                    lHasPlus = index(cValue,"+") > 0
                    cValue   =  quoter(cValue,"'")
                    . 
            end. 
            else 
                cValue = FieldValues[iField]:Value:ToString().
                           
           /* if hasspaceorplus use field name in expression so we can safely decode bad encoders like certain REST testing tools...
              some REST tools will encode space as +, but not if there already is a + 
              this is indeterministic since a value with + and no space could origin from both space and +
           */     
           if extent(FieldValues) = 1 and lHasPlus = false and lUseLabel = false and UseLabelForOne = false then 
           do:
               cRowKey = cValue.
           end.
           else if luselabel = false then
           do:
               cRowKey = cRowKey 
                           + (if cRowKey = "" then "" else cPredicateSep)
                           + subst("&1&2&3",
                                   FieldValues[iField]:name,
                                   cExpressionSep,
                                   cValue
                                  ).
           end.
           else do:
               
               cRowKey = cRowKey 
                           + (if cRowKey = "" then "" else cPredicateSep)
                           + subst("&1 &2",
                                   StringUtil:CamelToWords(FieldValues[iField]:name),
                                   cValue
                                  ).
           end.
           
       end.                              
       return cRowKey. 
    end method.
    
    method public override logical Equals(pObject as Object):
        define variable i as integer no-undo.
        define variable J as integer no-undo.
        define variable oOtherFieldValues as IFieldValue extent no-undo.
        define variable lOk as logical no-undo.
        
        if super:Equals(pObject) then
            return true.
        if type-of(pObject,IKey) = false then
            return false.
        
        oOtherFieldValues = cast(pObject,IKey):FieldValues.
        // this is not as expensive as it looks the number of fields in a key is low and in most cases 1.       
        outerloop:
        do i = 1 to extent(FieldValues):
            do j = 1 to extent(oOtherFieldValues):
                // if match was found check this objec's next fieldvalue  
                if FieldValues[i]:Equals(oOtherFieldValues[j]) then 
                   next outerloop.        
            end.
            // if none of the oOtherFieldValues is a match then this is not equals  
            return false.
        end.
        // if we get here all fields are equals (also if both are empty)
        return true.
        
    end method.    
    
    
end class.
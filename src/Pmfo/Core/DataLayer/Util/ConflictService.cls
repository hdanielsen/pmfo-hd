 
 /*------------------------------------------------------------------------
    File        : ConflictService
    Purpose     : Refine and manage response for locking conflicts 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Nov 25 08:59:20 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Util.BufferUtil from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.DataLayer.Util.IConflictService from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.Util.ConflictService implements IConflictService: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ConflictService (  ):
        super ().
    end constructor.
    
    method public character GetChangeLabels(phDatafields as handle extent).
        define variable hAfterBuffer as handle no-undo.
        define variable i            as integer no-undo.
        define variable cLabels      as character no-undo.
        define variable cDlm         as character no-undo. 
        
        do i = 1 to extent(phDataFields): 
            hAfterBuffer = phDataFields[i]:buffer-handle:after-buffer.
            if phDataFields[i]:buffer-value <> hAfterBuffer:buffer-field(phDataFields[i]:name):buffer-value then
            do: 
                cLabels = cLabels + cDlm + BufferUtil:GetFieldLabel(phDataFields[i]).
                cDlm = ",". 
            end.    
        end.
        return StringUtil:GetMessageFromEntries(cLabels, true).
        
    end method.
    
    method public character GetChangesFormatted(phDatafields as handle extent,phsourcefields as handle extent):
        define variable i as integer no-undo.
        define variable cLabel       as character no-undo init "Conflict Fields".
        define variable cLeftLabel   as character no-undo init "Client Value&1 Before Edit".
        define variable cRightLabel  as character no-undo init "Current Database Value".
        define variable iMaxLabel    as integer no-undo.
        define variable iMaxLeft     as integer no-undo.
        define variable iMaxRight    as integer no-undo.
        define variable cLabels      as character extent no-undo.
        define variable cString      as character no-undo.
        define variable cLeftValue   as character no-undo.
        define variable hAfterBuffer as handle no-undo.
        define variable lChanged     as logical extent no-undo.
        
        if extent(phDataFields) > 1 then 
            assign 
                cLeftLabel = subst(cLeftLabel,"s")
                cRightLabel = cRightLabel + "s"
            .
        else 
            cLeftLabel = subst(cLeftLabel).
        
        assign
            extent(cLabels) = extent(phDataFields)
            extent(lChanged) = extent(phDataFields)  
            iMaxLeft  = length(cLeftLabel)
            iMaxRight = length(cRightLabel)
            iMaxLabel  = length(cLabel)
        .
        
        do i = 1 to extent(phDataFields): 
            hAfterBuffer = phDataFields[i]:buffer-handle:after-buffer.
            lChanged[i]  = phDataFields[i]:buffer-value <> hAfterBuffer:buffer-field(phDataFields[i]:name):buffer-value.
        end.
        
        
        do i = 1 to extent(phDataFields):
            if lChanged[i] then 
                assign
                    cLabels[i] = BufferUtil:GetFieldLabel(phDataFields[i]) + ":"
                    iMaxLabel  = max(iMaxLabel,length(cLabels[i])) // + 1 colon 
                    iMaxLeft   = max(iMaxLeft,length(phDataFields[i]:buffer-value))  // 1 for space. 
                    iMaxRight  = max(IMaxRight,length(phSourceFields[i]:buffer-value)).
                    .
        end.
        
        cString = subst("~n~n&1&2 &3&4 &5",
                        cLabel,
                        fill(" ",iMaxLabel - length(cLabel)),
                        cLeftLabel, 
                        fill("X",iMaxLeft - length(cLeftLabel)),
                        cRightLabel
                        ). 
        cString = subst("&1~n&2 &3 &4",
                        cString,
                        fill("-",iMaxLabel),
                        fill("-",iMaxLeft),
                        fill("-",iMaxRight)
                        ).                 
                        
        do i = 1 to extent(phDataFields):
            if lChanged[i] then
            do:
                assign
                    cLeftValue = subst("&1 - &2",hAfterBuffer:buffer-field(phDataFields[i]:name):buffer-value,phDataFields[i]:buffer-value) 
                    cString = subst("&1~n&2 &3&4 &5&6",
                                    cString,  
                                    cLabels[i],
                                    fill(" ",iMaxLabel - length(cLabels[i])),
                                    cLeftValue, 
                                    fill(" ",iMaxLeft - length(cLeftValue)),
                                    phSourceFields[i]:buffer-value
                                    )
                   .
            end.                 
        end.
        cString = subst("&1~n&2 &3 &4~n",
                        cString,
                        fill("-",iMaxLabel),
                        fill("-",iMaxLeft),
                        fill("-",iMaxRight)
                        ). 
        return cString.
        
        
    end method.     
     
    
end class.
 
 /*------------------------------------------------------------------------
    File        : ModuloQueryPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : ^/26/2024
    Notes       : Use for modulo expression  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.BusinessLogic.ModuloQueryPredicate from propath.
using Pmfo.Core.Common.ModuloQueryOperatorEnum from propath.
using Pmfo.Util.QueryUtil from propath.

class Pmfo.Core.BusinessLogic.ModuloQueryPredicate inherits QueryPredicate: 
/*    define override property NotImplementedReasonTemplate as character no-undo                                                                                             */
/*        init "&1 property is not implemented. Use &2 instead. The predicate is only used for dynamic queries and string is sufficient and ensures safe dates and decimals."*/
/*    get.                                                                                                                                                                   */
/*                                                                                                                                                                           */
    define public property ModuloBase     as integer no-undo get. set. 
    define public property ModuloOperator as ModuloQueryOperatorEnum no-undo get. set. 
       
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param character value */
    constructor public ModuloQueryPredicate (input pFieldName  as character,
                                             input pModuloBase as integer,
                                             input pOperator   as ModuloQueryOperatorEnum,
                                             input pValue      as integer):
         super(pFieldname,string(pValue)).   
         ModuloBase = pModuloBase. 
         ModuloOperator = pOperator.                                    
    end constructor.
    
    method override public Progress.Lang.Object Clone(  ):
        return new ModuloQueryPredicate(FieldName,ModuloBase,ModuloOperator,integer(StringValue)).
    end method.
    
    
    // join is not included in tostring - it is used in QueryGroup when more than one predicate is shown 
    method override public character ToString():
        return ToString(?,false). 
    end method.
    
    method public override character ToString(phBuffer as handle,plUseSerializeName as logical):
        return ToString(phBuffer,plUseSerializeName,false).
    end method.     
    
    method public override character ToString(phBuffer as handle,plUseSerializeName as logical, plStripParenthesis as logical):
        define variable cSerializeName as character no-undo.
        define variable cOperator      as character no-undo.
        // buffer is for message use 
        if valid-handle(phBuffer) then
        do: 
            if plUseSerializeName then
            do:
                cSerializeName = phBuffer:buffer-field(FieldName):serialize-name no-error.
                if cSerializeName = ? or cSerializeName = "" then
                    cSerializeName = FieldName.
            end.
            cOperator = QueryUtil:GetOperatorText(ModuloOperator:ToString()).       
        end.
        else
            cOperator = lc(ModuloOperator:ToString()). 
       
       
        return subst('&1 modulo &2 &3 &4',
                                       if cSerializeName > "" then cSerializeName else FieldName,
                                       ModuloBase,
                                       cOperator,
                                       StringValue).
    end method.    
end class.
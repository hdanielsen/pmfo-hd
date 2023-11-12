 
 /*------------------------------------------------------------------------
    File        : Parameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 08:38:31 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.DataTypeHelper from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

class Pmfo.Core.Common.Parameter implements IParameter: 
    define public property Name         as character no-undo get. protected set.
    define public property DataType     as DataTypeEnum no-undo get. protected set.
    
    define public property IsPrimitive      as logical init ? no-undo 
        get():
            if IsPrimitive = ? then
                IsPrimitive = DataTypeHelper:IsPrimitive(DataType).
            return IsPrimitive.    
        end get. 
        private set.  
    
    define public property IsArray as logical init ? no-undo 
        get():
            define variable cMask as character no-undo.
            if IsArray = ? then 
            do:
               cMask = DataTypeHelper:GetMask(DataType).
               IsArray = (entry(num-entries(string(cMask), ' ':u), cMask, ' ':u) eq 'extent':u).
            end.
            return IsArray.    
        end get. 
        private set. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public Parameter (pname as character, pdatatype as character  ):
        this-object (pName, DataTypeEnum:GetEnum(pDataType)).
        catch e as Progress.Lang.Error :
            Undo, throw new IllegalArgumentError(subst("Parameter '&1' Datatype '&2' is not valid",pname,pdatatype),e).    
        end catch.
    end constructor.
    
    constructor public Parameter (pname as character, pdatatype as DataTypeEnum ):
        super().
        Assert:NotNull(pDataType,"DataType"). 
        Assert:IsTrue(DataTypeHelper:IsPrimitive(pDataType),subst("DataType '&1' is not a primitive data type",pDatatype)). 
        Name = pname.
        DataType =pDataType.
    end method.
    
    method public override  character ToString():
        return right-trim(subst("&1 &2 &3",
                                Name,
                                // keep it short since it is used in Signature tostring
                                replace(DataType:ToString(),"Array",""),
                                if IsArray then "[]" else "") 
                        ).
        
        
    end method.     
    
end class.
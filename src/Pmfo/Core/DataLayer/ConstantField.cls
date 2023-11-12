
/*------------------------------------------------------------------------
    File        : ConstantField
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 09/10/2022
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.
 
using Ccs.Common.Support.ICharacterHolder from propath.
using Ccs.Common.Support.IDateHolder from propath.
using Ccs.Common.Support.IDateTimeHolder from propath.
using Ccs.Common.Support.IDateTimeTzHolder from propath.
using Ccs.Common.Support.ILongcharHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using OpenEdge.Core.LogicalValue from propath.
using Pmfo.Core.Common.CharacterHolder from propath.
using Pmfo.Core.Common.IntegerHolder from propath.
using Pmfo.Core.DataLayer.IConstantField from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Progress.Lang.Object from propath.
 

class Pmfo.Core.DataLayer.ConstantField implements IConstantField:  
    
    define protected property Holder as IPrimitiveHolder no-undo get. set.
    
    define public property SourceName       as character no-undo get. protected set.
    define public property SourceBufferName as character no-undo get. protected set.
    define protected property SourceFullName   as character no-undo get. protected set.
    define public property QueryExpressionValue     as character no-undo 
        get():
            case Holder:GetCLass():
                when get-class (ICharacterHolder) 
                    or
                when get-class(IDateHolder)  
                    or
                when get-class(IDateTimeHolder) 
                   or
                when get-class(IDateTimeTzHolder) then 
                    return quoter(Holder:TosTring()). 
                
                when get-class (ILongcharHolder) then
                do:
                   do on error undo, throw :
                      return quoter(Holder:TosTring()). 
                      catch e as Progress.Lang.Error :
                          return quoter(substr(Holder:ToString(),1,1000) + "..more....").         
                      end catch.
                   end.  
                end.     
                otherwise 
                    return Holder:ToString().
            end.    
        end.        
    
    constructor public ConstantField (pcValue as character, phDbBuffer as handle, pcSourceName as character   ):
        this-object(new CharacterHolder(pcValue),phDbBuffer,pcSourcename). 
    end constructor.
    
    constructor public ConstantField (piValue as integer, phDbBuffer as handle, pcSourceName as character   ):
        this-object(new IntegerHolder(piValue),phDbBuffer,pcSourcename). 
    end constructor.
    
    constructor public ConstantField (plValue as logical, phDbBuffer as handle, pcSourceName as character   ):
        this-object(new LogicalValue(plValue),phDbBuffer,pcSourcename). 
    end constructor.
    
    constructor protected ConstantField (pHolder as IPrimitiveHolder, phDbBuffer as handle, pcSourceName as character   ):
        Holder = pHolder. 
        SourceBufferName = phDBBuffer:name.
        SourceName = pcSourceName.
        SourceFullName = subst("&1.&2",SourceBufferName,SourceName).
    end constructor.
    
    method public logical Assign(phDbfield as handle).
        case phDbField:data-type:
            when 'character' then 
                phDbfield:buffer-value = PrimitiveHolderUtil:GetCharacterValue(Holder).
            when 'integer' then 
                phDbfield:buffer-value = PrimitiveHolderUtil:GetIntegerValue(Holder).
            when 'logical' then 
                phDbfield:buffer-value = PrimitiveHolderUtil:GetLogicalValue(Holder).
            otherwise        
                undo, throw new NotImplementedError(subst("Save of Constant field for Data Type",phDbfield:data-type)).           
            end.
    end method. 
    
    // The value is not Identifer for this - need to ensure only one in collections
    method public override logical Equals(pObject as Object): 
        if type-of(pObject,IConstantField) then 
             return cast(pObject,IConstantField):SourceBufferName = this-object:SourceBufferName 
                    and
                    cast(pObject,IConstantField):SourceName = this-object:SourceName            
             .
        return false. 
    end.      
 
    method public override character ToString(): 
        return SourceFullName.
    end.     
    
end class.
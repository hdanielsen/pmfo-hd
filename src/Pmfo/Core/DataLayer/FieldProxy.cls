
/*------------------------------------------------------------------------
    File        : IFieldProxy
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Sep 26 06:56:59 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.
 
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.DataLayer.AbstractFieldProxy from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.BufferUtil from propath.
 

class Pmfo.Core.DataLayer.FieldProxy inherits AbstractFieldProxy:  
 
    define public override property StringValue as character no-undo 
        get():
            return string(this-object:Field:buffer-value).
        end.
    
    constructor public FieldProxy (pcFieldName as character, pcSourceName as character):
        this-object(pcFieldName,pcSourceName,false).
    end constructor.
    
    constructor public FieldProxy (pcFieldName as character, pcSourceName as character, plReadOnly as logical):
        super(pcFieldName,pcSourceName,plReadOnly).
    end constructor.
    
    // DEPREDCATE and use above
    constructor public FieldProxy (phField as handle,phDbBuffer as handle, pcSourceName as character   ):
        super(phField,phDbBuffer,pcSourceName).
    end constructor.
    
   // constructor protected FieldProxy (phField as handle,pcDbBufferName as character, pcSourceName as character,pcDataType as character):
     //   super (phfield,pcDbBufferName,pcSourceName,pcDataType).
   // end constructor.
    
    method public override logical Save():
        
        if CheckAndAssertSave() then 
        do:
           // Progress old magic runtime type conversion will box data type if possible 
            // a proxy should only be defined if the data type can be converted automatically 
            // override for other cases use customFieldProxy or just override this 
            SourceField:buffer-value(SourceExtent) = this-object:Field:buffer-value.
            return  true.
        end.    
        else 
            return false.
    end method.    
    
    method protected override logical Conflict(phFld as handle):
        // Progress old magic runtime type conversion will box data type if possible 
        // (a proxy should only be defined if the data type can be converted automatically) 
        return phFLd:buffer-value <> SourceField:buffer-value(SourceExtent).    
    end method.
         
    method public override IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate):
        define variable i             as integer no-undo.
        define variable lZero         as logical    no-undo.
        define variable oQueryGroup   as QueryGroup no-undo.
        define variable cStringValues as character  extent no-undo.
        
        // The main use case is to map other data types to a db character field
        if SourceDataType = "character" 
        // for equality operators we can avoid the typing of the db field and use index   
        and lookup(pPredicate:Operator:ToString(),"Eq,InList") > 0
        // we currently skip dates as the format in character fields can have yyyy and yy
        // logical is not managed here but in LogicalFieldProxy)
        // (@TODO we can add both similar to 0
        and lookup(this-object:Field:data-type,"integer,decimal,int64") > 0 then
        do: 
            if extent(pPredicate:StringValues) > 0 then
            do:
                // add room for blank 
                extent(cStringValues) = extent(pPredicate:StringValues) + 1.
                do i = 1 to extent(pPredicate:StringValues):
                    // if value is 0 (or 0.0) we add blank to expression since int("") and dec("") both are 0 
                    // it is not necessary to do this for GT since 0 will exclude blank
                    if not lZero then
                    do: 
                        case this-object:Field:data-type:
                            when "integer" then
                                lZero = integer(pPredicate:StringValues[i]) = 0.
                            when "int64" then
                                lZero = int64(pPredicate:StringValues[i]) = 0.
                            when "decimal" then
                                lZero = decimal(pPredicate:StringValues[i]) = 0.
                        end.
                    end.
                    cStringValues[i] = pPredicate:StringValues[i].  
                end.      
                // if we found zero add blanl to the new array and use that
                if lZero then 
                do:
                    cStringValues[extent(cStringValues)] = "".
                    return new QueryPredicate(SourceFullName,pPredicate:Operator,cStringValues).
                end.    
                else
                    return new QueryPredicate(SourceFullName,pPredicate:Operator,pPredicate:StringValues).
            end.
            else do:
                // if value is 0 (or 0.0) we add blank to expression since int("") and dec("") both are 0 
                // it is not necessary to do this for GT since 0 will exclude blank
                case this-object:Field:data-type:
                    when "integer" then
                        lZero = integer(pPredicate:StringValue) = 0.
                    when "int64" then
                        lZero = int64(pPredicate:StringValue) = 0.
                    when "decimal" then
                        lZero = decimal(pPredicate:StringValue) = 0.
                end.
                if lZero then
                do:
                    oQueryGroup = new QueryGroup().
                    oQueryGroup:AddEntry(new QueryPredicate(SourceFullName,pPredicate:Operator,pPredicate:StringValue)).
                    oQueryGroup:AddEntry(new QueryPredicate(JoinEnum:Or,SourceFullName,pPredicate:Operator,"")).
                    return oQueryGroup.
                end.    
                else 
                    return new QueryPredicate(SourceFullName,pPredicate:Operator,pPredicate:StringValue).
            end.    
        end.
        // this is not the normal use case, but could in theory happen.  
        // A fieldproxy could be used to create on the fly mapping for varying types 
        else if SourceDataType = "character" and this-object:Field:data-type = "character" then
        do: 
            if extent(pPredicate:StringValues) > 0 then
                return new QueryPredicate(SourceFullName,pPredicate:Operator,pPredicate:StringValues).
            else
                return new QueryPredicate(SourceFullName,pPredicate:Operator,pPredicate:StringValue).
        end.
        // use data-type(dbfield) - this means that this will not utilize an index on the field
        // it is theoreticall possible to do the data type conversion to the source type on the right-side, but
        // would fail if the stored string value is not an exact match 
        // . i,e db values  int("0") and int("") are both 0, but a right hand side integer would always be 0 and not find ""      
        else do:
            if extent(pPredicate:StringValues) > 0 then
                return new QueryPredicate(subst("&1(&2)",this-object:Field:data-type,SourceFullName),pPredicate:Operator,pPredicate:StringValues).
            else
                return new QueryPredicate(subst("&1(&2)",this-object:Field:data-type,SourceFullName),pPredicate:Operator,pPredicate:StringValue).
        end.    
    end.
    
    method public override IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ):
        return new QuerySortEntry(subst("&1(&2)",this-object:Field:data-type,SourceFullName),pSort:SortOrder).
    end.
    
    method override void SetBufferValue(pValue as character):
        if pvalue = "" then
        do: 
            case this-object:Field:data-type: 
                when "integer" or when "int64" or when "decimal" then 
                     pValue = "0".  
            end case.       
        end.    
        this-object:Field:buffer-value = pValue.
        catch e as Progress.Lang.Error :
            define variable cErrorExplanation as character no-undo.
            
            if e:GetMessageNum(1) = 132 then 
                cErrorExplanation = subst("The field &1 is in a unique index in the Entity, but the database has more than one record in the &2 table with value &3 in the &4 field.",
                                          this-object:Field:name,
                                          SourceBufferName,
                                          if SourceDataType = "character" then quoter(pValue) else pValue,
                                          Sourcename
                                          ).
            else 
                cErrorExplanation  = e:Getmessage(1).
            undo, throw new IllegalStateError(subst("Field Proxy cannot assign value &1 to &2 field &3 from database field &4.&5~n&6",
                                                                   if SourceDataType = "character" then quoter(pValue) else pValue,
                                                                   this-object:Field:data-type,
                                                                   this-object:Field:name,
                                                                   SourceBufferName,
                                                                   Sourcename,
                                                                   cErrorExplanation),
                                             e).     
        end catch.
    end.
    
    method override void SetBufferValue(phBuffer as handle):
        define variable hFld as handle no-undo.
        hfld = phBuffer:buffer-field(SourceName).
        SetBufferValue(hfld:buffer-value(SourceExtent)).
    end.  
        
    method override void SetBufferValue(pValue as integer):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as int64):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as logical):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as decimal):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as date):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as datetime):
        this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as datetime-tz):
        this-object:Field:buffer-value = pValue.
    end.    
    
end class.
 
 /*------------------------------------------------------------------------
    File        : ExpandOptionsParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 04 23:37:55 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.StringUtil from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Common.AggregationMethodEnum from propath.
using Pmfo.Core.Common.Aggregate from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.Set from propath.

class Pmfo.Core.BusinessLogic.Parser.ApplyOptionsParser:
    define protected property IsResolved as logical no-undo get. set.
      
    define public property GroupBy              as character  no-undo get. set. 
    define public property HasAggregate         as logical    no-undo get. set. 
    define public property InnerJoins           as character  no-undo get. set. 
    define public property TransformationFilter as character  no-undo get. set. 
    
    define public property Aggregates      as ICollection  no-undo 
        get():
            if not valid-object(Aggregates) then
                Aggregates = new Set().
            return Aggregates.     
        end.     
        set(pAggregate as ICollection):
            Aggregates = pAggregate.
            HasAggregate = true.
        end. 
    
        /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ApplyOptionsParser (pcApply as character):
        super ().
        if pcApply > "" then
            ResolveProperties(pcApply).
    end constructor.
     
    method public void ResolveProperties(pcApply as character):
        define variable cOdataOptions as character init "aggregate,topcount,topsum,toppercent,bottomcount,bottomsum,bottompercent,identity,concat,groupby,filter,expand,search" no-undo.
        define variable i as integer no-undo. 
        define variable oOptions       as IStringStringMap no-undo.
        define variable iParenthesis   as integer no-undo.
        define variable iComma         as integer no-undo.
        define variable cOption        as character no-undo.
        define variable iOption        as integer no-undo.
        define variable cExpression    as character no-undo.
        define variable cRemainder as character no-undo.
        
        iParenthesis = index(pcApply,"(").
        if iParenthesis = 0 then  
            undo, throw new IllegalArgumentError(subst("apply=&2'. The the apply value must have an option with an expression in parenthesis.",pcApply)).
        
        cOption = trim(substr(pcApply,1,iParenthesis - 1)).  
        
        case cOption:
            when "filter" then 
            do: 
                // temporary only allow plant id transformation filter (for server)  
               //  odata: 
                // The server filter for apply is supposed to be passed as an apply aggregation while the filter is supposed to 
                // reference data in the aggregated result so as for now we only allow plant id 
                // TODO - integrate filter parsing from ExpandOptionParser (probably move to abstract superclass or separate parser to be called from thus and ExpandOptionParser)  
                cExpression = StringUtil:ExtractParenthesis(substr(pcApply,1),true).
                if cExpression = ? then 
                    undo, throw new IllegalArgumentError(subst("apply=&1. The expression cannot be parsed",pcApply)). 
                if cExpression matches "plantid *eq*"
                and index(cExpression,"'") = 0 
                and index(cExpression,'"') = 0 
                and index(cExpression,'(') = 0 
                and index(cExpression,')') = 0 
                and index(cExpression,' and ') = 0 
                and index(cExpression,' or ') = 0 then
                do: 
                   TransformationFilter = cExpression. // QueryRequestbuilder will merge this with server filter
                   cRemainder = substr(pcapply,iParenthesis + 2 + length(cExpression)).
                   if cRemainder begins "/" then
                       ResolveProperties(substr(cRemainder,2)).      
                   else if cRemainder > "" then
                       undo, throw new IllegalArgumentError(subst("apply=&1. Invalid expression '&2' found after &3 parenthesis expression.",
                                                                 pcApply,
                                                                 cRemainder,
                                                                 cOption)).
                        
                end.
                else 
                    undo, throw new NotImplementedError(subst("apply=&1(&2) that has any other predicate than 'plantid eq' and/or complex expression",cOption,cExpression)). 
                 
            end.
            when "groupby" or when  "aggregate" then 
            do: 
                cExpression = StringUtil:ExtractParenthesis(substr(pcApply,1),true).
                if cExpression = ? then 
                    undo, throw new IllegalArgumentError(subst("apply=&1. The expression cannot be parsed",pcApply)). 
                
                cRemainder = substr(pcapply,iParenthesis + 2 + length(cExpression)).
                if cRemainder begins "/" then
                    undo, throw new NotImplementedError(subst("apply=&1 with option(s) separated by '/'",cOption)). 
                else if cRemainder > "" then
                do:
                    undo, throw new IllegalArgumentError(subst("apply=&1. Invalid expression '&2' found after &3 parenthesis expression.",
                                                               pcApply,
                                                               cRemainder,
                                                               cOption)). 
                end.
                case cOption:
                    when "groupby" then 
                       ExtractGroups(cExpression).
                    when "aggregate" then 
                       ExtractAggregates(cExpression).   
                end.
            end.        
            otherwise do:
                iOption = lookup(cOption,cOdataOptions).
                if iOption > 1 then 
                    undo, throw new NotImplementedError(subst("apply option '&1'",entry(iOption,cOdataOptions))). 
                else      
                    undo, throw new IllegalArgumentError(subst("apply=&1. Option '&2' is not valid",pcApply,cOption)). 
            end.     
        end case.
    end method.
    
    method protected void ExtractAggregates(pcAggregates as character):
       define variable i as integer no-undo.         
       define variable iWord as integer no-undo.         
       define variable cWord as character no-undo.         
       define variable cAggregate as character no-undo.
       define variable iState as integer no-undo.
       define variable lCountFunction as logical no-undo.
       define variable cProperty as character no-undo.
       define variable cLabel as character no-undo. 
       define variable cQualifier as character no-undo.
       define variable oAggregateMethod as AggregationMethodEnum no-undo.
       
       // there are two valid expressionsL  
       // prop with sum as label 
       // count as label
       
       do i = 1 to num-entries(pcaggregates): 
          caggregate = entry(i,pcaggregates).
      
          lCountFunction = false.
          cProperty = "".
          cLabel   = "".
          oAggregateMethod = ?.
          do iWord = 1 to num-entries(cAggregate,""):
              cWord = entry(iWord,caggregate,"").
              case iWord:  
                  when 1 then 
                  do:
                      cProperty = cWord.
                      lCountFunction = cWord = "count".
                  end.   
                  when 2 then 
                  do:
                      if (cWord <> "with" and cWord <> "as") 
                      or (cWord = "as" and lCountFunction = false) then
                      do: 
                          if lCountFunction then 
                              undo, throw new IllegalArgumentError(subst("Invalid aggregate expression '&1'. '&2' encountered instead of 'with' or 'as'",cAggregate,cWord)).
                          else  
                              undo, throw new IllegalArgumentError(subst("Invalid aggregate expression '&1'. '&2' encountered instead of 'with'",cAggregate,cWord)).
                          
                      end.
                      if cWord <> 'as' then 
                          lCountFunction = false.    
                  end.
                  when 3 then 
                  do:
                      if lCountFunction then
                      do: 
                          cLabel = cWord.
                          leave.
                      end.
                      do on error undo, throw:
                          oAggregateMethod = AggregationMethodEnum:GetEnum(cWord).
                          catch e as Progress.Lang.Error :
                             undo, throw new IllegalArgumentError(subst("Invalid aggregate expression '&1'. '&2' is not a valid aggregation method'",cAggregate,cWord)).
                              
                          end catch.
                      end.                      
                      if AggregationMethodEnum:CountDistinct:Equals(oAggregateMethod) then 
                          undo, throw new NotImplementedError(subst("groupby with aggregate method &1",oAggregateMethod:ToString())).
                                                  
                  end.
                  when 4 then 
                  do:
                      if cWord <> "as" then
                         undo, throw new IllegalArgumentError(subst("Invalid aggregate expression '&1'. '&2' encountered instead of 'as'",cAggregate,cWord)).
                  end.  
                  when 5 then 
                  do:
                      cLabel = cWord.
                  end.  
              end.   
          end.  
          if cProperty = "" 
          or cLabel = ""
          or (lCountFunction = false and not valid-object(oAggregateMethod)) then 
             undo, throw new IllegalArgumentError(subst("Invalid aggregate expression '&1'",cAggregate)).
          
          if num-entries(cProperty,"/") > 2 then
          do:
              undo, throw new NotImplementedError(subst("Aggregate of property '&1' with more than one entity qualifier",cProperty)).
          end.
          else if num-entries(cProperty,"/") = 2 then
          do:
              cQualifier = entry(1,cProperty,"/").
              if lookup(cQualifier,InnerJoins) = 0 then 
                  StringUtil:AppendEntry(InnerJoins, cQualifier).
          end.
          // Error will be thrown when processing if serializename is not uniuqe or property is not numeric    
          Aggregates:Add(new Aggregate(cProperty,oAggregateMethod,cLabel)).       
       end.     
    end method.     
    
    method protected void ExtractGroups(pcGroups as character):
        define variable i          as integer no-undo.         
        define variable cGroups    as character no-undo.
        define variable cRemainder as character no-undo.
        
        pcGroups = trim(pcGroups).
        
        if pcGroups begins "(" then 
        do:
            cGroups = StringUtil:ExtractParenthesis(substr(pcGroups,1),true).
            GroupBy = GetGroups(StringUtil:ExtractParenthesis(substr(pcGroups,1),true),true).
           
            cRemainder = substr(pcGroups,1 + 2 + length(cGroups)).  
            if cRemainder > "" then
            do: 
                if cRemainder begins ",aggregate" then 
                    ResolveProperties(substr(cRemainder,2)).
                else
                    undo, throw new IllegalArgumentError(subst("apply=groupby(&1). Invalid expression '&2' found after properties (&3)",pcGroups,cRemainder,GroupBy)).
            end. 
        end.
        else 
            GroupBy = GetGroups(pcGroups,false).
        
    end method.
    
    method protected character GetGroups(pcGroups as character,plInparenthsisAlready as logical):
        define variable i as integer no-undo.  
        define variable cGroups as character no-undo.
        define variable cGroup as character no-undo.
        do i = 1 to num-entries(pcGroups):
            cGroup = entry(i,pcGroups).
            if cGroup begins "aggregate" and index(cGroup,"(") > 0 then 
            do:
               if plInparenthsisAlready then
                   undo, throw new IllegalArgumentError(subst("Invalid aggregate option encountered in groupby (&1). The aggregate option(s) should be spefified separated by comma after the parenthesized properties.",pcGroups)).
               else
                   undo, throw new IllegalArgumentError(subst("Invalid aggregate option encountered in groupby &1.In order to use groupby with aggregate the groupby properties must be in parenthesis with comma separated aggregate option(s) after the parenthesized properties.",pcGroups)). 
            end.
            StringUtil:AppendEntry(cGroups,trim(cGroup)). 
        end.        
        return pcGroups.
    end method.
      
end class.
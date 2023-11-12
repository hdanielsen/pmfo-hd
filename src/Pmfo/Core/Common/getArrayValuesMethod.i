/*------------------------------------------------------------------------
    File        : getArrayValuesMethod.i
    Author(s)   : hdaniels
  ----------------------------------------------------------------------*/
    &if "{3}" <> "" &then 
       &scop methodname {3}
    &else
       &scop methodname Get{1}Values 
    &endif 
    
  
    method public {1} extent {&methodname}(pcName as char). 
        define variable oValue  as Object no-undo.
        define variable vValues as {1} extent no-undo.
        oValue = HolderMap:Get(new String(pcName)).
        if valid-object(oValue) then
        do on error undo, throw:
           vValues = PrimitiveHolderUtil:{&methodname}(cast(oValue,{2})).
           catch e as Progress.Lang.Error :
              undo, throw new Pmfo.Core.Error.IllegalArgumentError(subst("Array '&1' could not be returned as &2",pcName,lc("{1}")),e).      
           end catch.
        end.
        return vValues.   
    end method.
    
     
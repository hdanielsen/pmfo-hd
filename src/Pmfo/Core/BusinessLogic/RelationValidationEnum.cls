
/*------------------------------------------------------------------------
    File        : RelationValidationEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Feb 26 08:24:05 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.RelationValidationEnum :  

    define enum Optional // must exist if not empty and not null
                NotNull  // empty is allowed (0 or blank), but not null
                NotEmpty // null is allowed, but not blank or 0  
                Mandatory. // NotNull,NotEmpty.
// TODO before use 
//- how to deal with 1. empty allowed in data and 2. empty not allowed  (even if it exist) 
// is this better done with flag enum?  
 
      /** examples - basic premise: only values that matches a key can be saved except ? or empty
      ?????
      NotNull 
      if foreignkey = ? then "please provide"
      else if not can-find(parent where parent.key = foreingkey) then "Parent $1 does not exist"
         - 1 
         - 0 if match
      
      NotEmpty
      if foreignkey = ? or foreignkey = 0  then "please provide"
      else if not can-find(parent where parent.key = foreingkey) then "Parent $1 does not exist"
         - 1
      
      Optional
      if foreignkey <> ? then if not can-find(parent where parent.key = foreingkey) then "Parent $1 does not exist"
         - ?    
         - 1
           
      if foreignkey <> ? and foreignkey <> 0 then if not can-find(parent where parent.key = foreingkey) then "Parent $1 does not exist"
         - ?
         - 0 no match necessary
         - 1
            
      
      if foreignkey = ? then "please provide"
      else if foreignkey <> 0 then if not can-find(parent where parent.key = foreingkey) then "Parent $1 does not exist"
         - 0 no match necessary
         - 1         
      
       **/
        
end enum.
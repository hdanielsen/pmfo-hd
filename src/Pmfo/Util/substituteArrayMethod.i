
/*------------------------------------------------------------------------
    File        : toStringArrayMethod.i
    Purpose     : 

    Syntax      : 

    Description : 

    Author(s)   : hdaniels
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/
   
    method public static character SubstituteArray(pcTemplate as character,pArray as {1} extent):
        case extent(pArray):
            when 1 then 
                return substitute(pcTemplate,pArray[1]).
            when 2 then 
                return substitute(pcTemplate,pArray[1],pArray[2]).
            when 3 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3]).
            when 4 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4]).
            when 5 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4],pArray[5]).
            when 6 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4],pArray[5],pArray[6]).
            when 7 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4],pArray[5],pArray[6],pArray[7]).
            when 8 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4],pArray[5],pArray[6],pArray[7],pArray[8]).
            when 9 then 
                return substitute(pcTemplate,pArray[1],pArray[2],pArray[3],pArray[4],pArray[5],pArray[6],pArray[7],pArray[8],pArray[9]).
            otherwise
                undo, throw new Pmfo.Core.Error.OutOfBoundsError ("Number of extents for SubstituteArrayMethod", 9).
                
        end.    
         
    end.    
    
    
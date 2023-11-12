 
 /*------------------------------------------------------------------------
    File        : DynamicDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 19:02:27 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.DataAccess.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.DynamicDataAccess inherits DataAccess: 
    define override public property Resource as character no-undo  
         get.
         protected set.
         
    constructor public DynamicDataAccess (pcname as char ):
        super ().
        Resource = pcname.       
    end constructor.
    
end class.
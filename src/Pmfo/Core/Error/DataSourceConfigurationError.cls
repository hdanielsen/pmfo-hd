 
 /*------------------------------------------------------------------------
    File        : DataSourceConfigurationError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Nov 20 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.Error.ApplicationError.
 

block-level on error undo, throw.

class Pmfo.Core.Error.DataSourceConfigurationError inherits ApplicationError: 
    constructor public DataSourceConfigurationError ( input pmsg as character ):
        super (input "Data Source Configuration Error: " + pmsg).
    end constructor.
     
end class.



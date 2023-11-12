 
 /*------------------------------------------------------------------------
    File        : MappedOperation
    Purpose     : Extend Mappedoperation for support of operations not backed by
                  gen and map data 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 17 11:05:09 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Net.HTTP.MethodEnum from propath.
using OpenEdge.Web.DataObject.MappedOperation from propath.
using OpenEdge.Web.DataObject.OperationOptions from propath.

class Pmfo.Core.Service.MappedOperation inherits MappedOperation: 
    
    /**
    @param service URI - service URI
    @param method - web method 
    @param class  - :TypeName is passed as first parameter to ServiceManager:Getservice() 
                    and used as target type to find OperationHandler from OpersationHandlerRegistry
    @param target name - resource - passed as second parameter to ServiceManager:Getservice()
    
    **/
    constructor public MappedOperation ( pServiceURI as character, pMethod as MethodEnum, pClass as Progress.Lang.Class, pTargetName as char):
        super (input pServiceURI, 
               input pMethod, 
               input 'application/json', 
               input pClass:TypeName,
               input pTargetName, 
               input pClass:TypeName, 
               input '', 
               input new OperationOptions(), 
               input 200).
    end constructor.
    
  
end class.
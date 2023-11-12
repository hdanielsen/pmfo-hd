
/*------------------------------------------------------------------------
    File        : IResource
    Purpose     : A resource is a application/businesslogoic object ot a data 
                  access object. 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jun 05 16:37:41 EDT 2019
    Notes       : The resource defines the public name used in the URL for the 
                  application layer. It is also the public name used for the 
                  dataaccess layer. It is public from the view point of the 
                  applicaion layer. A default config will have the same resource
                  name in both layers.   
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Common.IDataService from propath.
using OpenEdge.Core.ISupportInitialize from propath.

interface Pmfo.Core.Common.IResource inherits IDataService, ISupportInitialize:  
    define public property Resource as character no-undo get.  
end interface.
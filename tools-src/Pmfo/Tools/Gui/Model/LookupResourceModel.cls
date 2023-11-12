 
 /*------------------------------------------------------------------------
    File        : LookupResourceModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 09 20:14:47 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 

block-level on error undo, throw.

using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Tools.Gui.Model.AbstractResourceModel from propath.
using Pmfo.Tools.Gui.Model.IResourceModel from propath.

class Pmfo.Tools.Gui.Model.LookupResourceModel inherits AbstractResourceModel implements IResourceModel: 
    
    // TODO - refactor away from ResourceModel or move this class to a higher level interface that does not have ot? - exists in IResourceModel since it ios used by WinDesign   
    define public property MonitorSources   as character extent no-undo get. set.
    
    constructor public LookupResourceModel (  ):
        super (input "LookupModel").
    end constructor.
   
    method override public void FetchData(  ):
        if valid-object(CodeTableModel) then
        do:
            CodeTableModel:AddCodes(this-object,?,input-output table resourceData by-reference).
            // MergeCodeTableSDOs(input-output table resourceData by-reference).
        end.
    end method.

    method override public void FetchData( input pcQuery as character ):
        undo, throw new NotimplementedError().
    end method.

    method override public void FetchData( input pcQuery as character, input pMonitor as Pmfo.Tools.Gui.Model.IMonitor ):
        undo, throw new NotimplementedError().
    end method.

    method override public void FetchData( input pcTables as character extent, input pcQueries as character extent ):
        undo, throw new NotimplementedError().
    end method.

end class.
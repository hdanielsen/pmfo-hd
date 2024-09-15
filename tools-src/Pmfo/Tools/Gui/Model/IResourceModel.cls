
/*------------------------------------------------------------------------
    File        : IResourceModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 06 22:32:29 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.IModel from propath.
 
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.

interface Pmfo.Tools.Gui.Model.IResourceModel inherits IModel:  
    {Pmfo/Tools/AppBuilder/resourcedata.i reference-only}
    define public property CodeTableModel as ICodeTableModel no-undo get. set.
    define public property AdoModelType   as Progress.Lang.Class no-undo get. set. 
    define public property DefinitionDir  as char no-undo get. set. 
    define public property NameService    as INameService no-undo get. set.
    define public property MonitorSources as character extent no-undo get. set.
    
    method public handle GetTable(pcTable as char).
    // call back for external objects that creates resources
    method public void AfterRow(input table resourcedata).
end interface.
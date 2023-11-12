 
 /*------------------------------------------------------------------------
    File        : TrackLoadOrderAction
    Purpose     : Use as action to track the order of rows when loading from JSON    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 08 13:27:44 EST 2023
    Notes       : 1. Define as action in class. 
                  2. Implement IJsonReadHandlerProvider and add GetGetJsonReadHandler that 
                     returns new JsonTrackSequenceHandler when this action is passed 
                  3. Implement an action method in the class that grabs LoadSequences  
                     list (that preserves the order) from the action and updates the data accordingly. 
                     One would typically call RemoveAction in the Updaterequest to manage 
                     the save as a regualr upodate in DataAccess           
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.
using Pmfo.Core.BusinessLogic.Action from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.BusinessLogic.ITrackLoadOrderAction from propath.

class Pmfo.Core.BusinessLogic.TrackLoadOrderAction inherits Action implements ITrackLoadOrderAction: 
    define public property LoadSequences as IList no-undo
        get(): 
            if not valid-object(LoadSequences) then 
                LoadSequences = new List(). 
            return LoadSequences.    
        end. 
        set.    
   
    constructor public TrackLoadOrderAction ( input pName as character ):
        super (input pName).
    end constructor.
   
    method override protected void CreateParameters():
        DefineSignature().
    end method.   
       
end class.
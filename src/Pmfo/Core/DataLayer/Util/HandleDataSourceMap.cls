 
 /*------------------------------------------------------------------------
    File        : HandleDataSourceMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 10 01:18:00 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.WidgetHandle from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.DataLayer.Util.HandleDataSourceMap /*inherits Map*/ implements IHandleDataSourceMap: 
    
    define protected property SourceMap as IMap no-undo 
        get():
            if not valid-object(SourceMap) then
                SourceMap = new Map().
            return SourceMap.   
        end. 
        set. 
    define public property Size as integer no-undo 
        get():
            return SourceMap:Size.
        end.   
    constructor public HandleDataSourceMap():
        super().
    end constructor.
    
    method public IDataSource Get(phBuffer as handle):
        return cast(SourceMap:Get(new WidgetHandle(phBuffer)),IDataSource).
    end method. 
        
    method public IDataSource Put(phBuffer as handle, pSource as IDataSource).
        return cast(SourceMap:Put(new WidgetHandle(phBuffer),pSource),IDataSource). 
    end. 
    
    method public logical ContainsKey(phBuffer as handle).
        return SourceMap:ContainsKey(new WidgetHandle(phBuffer)).
    end method.
    
    method public IDataSource extent Sources(). 
        return  cast(SourceMap:Values:ToArray(),IDataSource).
    end method.
    
    destructor public HandleDataSourceMap():
        define variable oSources as ICollection no-undo. 
        define variable oIter    as IIterator  no-undo. 
        define variable oObj     as Object no-undo. 
        oSources = SourceMap:Values.
        oIter = oSources:Iterator().
        do while oIter:HasNext():
            oObj = oIter:Next().
/*            if type-of(oObj,IUpdateTarget) then                                  */
/*            do:                                                                  */
/*                 BeforeUpdate:Unsubscribe(cast(oObj,IUpdateTarget):BeforeUpdate).*/
/*                 AfterUpdate:Unsubscribe(cast(oObj,IUpdateTarget):AfterUpdate).  */
/*            end.                                                                 */
            
            delete object oObj.
        end.    
        delete object SourceMap.
    end destructor.
    
end class.
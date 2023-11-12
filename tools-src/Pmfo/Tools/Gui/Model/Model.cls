 
 /*------------------------------------------------------------------------
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 18 2011
    Notes       : 
  ----------------------------------------------------------------------*/
routine-level on error undo, throw.

using Progress.Lang.* from propath.

using Pmfo.Tools.Gui.Model.IModel from propath.
using Pmfo.Tools.Gui.Model.Query from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.

class Pmfo.Tools.Gui.Model.Model abstract implements IModel: 
     
    define private variable dataQueries    as Query  extent no-undo.
    define private variable cTables        as character no-undo.
    /* passed to serviceadpater DataAccessFactory */
    define public property EntityName as character no-undo get. set.
  
    define protected property DatasetHandle as handle no-undo get. set.
	
    /*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
		
	constructor public Model (name as char,datasethdl as handle):
		define variable i as integer no-undo.
		super ().		
		DatasetHandle = datasethdl.
		extent(dataQueries) = DatasetHandle:num-buffers.
		EntityName = name.
		/* lookup  */
		do i = 1 to DatasetHandle:num-buffers:
		    cTables = cTables + (if i = 1 then "" else ",") + DatasetHandle:get-buffer-handle(i):name.
        end.        
    
	end constructor.
    
    method public Query  GetQuery(pcTable as char):
        define variable i as integer no-undo.
        
        i = lookup(pcTable,cTables).
        if i = 0 then 
            undo, throw new AppError("Table " + pcTable + " not found in Model " + EntityName).
        if not valid-object(dataQueries[i]) then
            dataQueries[i] = CreateQuery(i).
        
        return dataQueries[i].
    end method. 
     
    method public Query GetQuery(i as int):
        if i > DatasetHandle:num-buffers then 
            undo, throw new AppError(string(i) +  " is out of range for num tables in Model " + EntityName).
        if not valid-object(dataQueries[i]) then
            dataQueries[i] = CreateQuery(i).    
        return dataQueries[i].
    end method.  
    
    method protected Query CreateQuery(pcTable as char):
        define variable newquery as Query no-undo.
        newquery = new Query(DatasetHandle:get-buffer-handle(pcTable)).
        return newquery.
    end method.
    
    method protected Query CreateQuery(i as int):
        define variable newquery as Query no-undo.
        newquery = new Query(DatasetHandle:get-buffer-handle(i)).
        return newquery.
    end method.
    
    method abstract public void  FetchData().
    method abstract public void  FetchData(pcQuery as character).
    method abstract public void  FetchData(pcQuery as character, pMonitor as IMonitor).
    method abstract public void  FetchData(pcTables as char extent,pcQueries as character extent).
   
    
   
	/*------------------------------------------------------------------------------
			Purpose: Remove the queries 																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	destructor public Model ( ):
        define variable i as integer no-undo.
        do i = 1 to extent(dataQueries):
            if valid-object(dataQueries[i]) then
                delete object dataQueries[i] .
        end.
	end destructor.
     
end class.
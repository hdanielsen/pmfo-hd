 
 /*------------------------------------------------------------------------
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Oct 15 22:25:47 EDT 2015
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.DataAdmin.DataAdminService from propath.
using OpenEdge.DataAdmin.IArea from propath.
using OpenEdge.DataAdmin.IExtent from propath.
using OpenEdge.DataAdmin.IAreaSet from propath.
using OpenEdge.DataAdmin.Error.DataAdminErrorHandler from propath.
using OpenEdge.DataAdmin.Lang.Collections.IIterator from propath.

class Pmfo.Tools.Util.AreaClient: 

	define public property DatabaseName as character init ? no-undo 
	get.
	private set. 

	define public property Service as DataAdminService no-undo 
	get.
	private set. 

	define public property ShowErrors as logical no-undo 
	get.
	set. 

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	constructor public AreaClient (  ):
		super ().
	end constructor.
	
    constructor public AreaClient (pDatabaseName as char):
        super ().
        DatabaseName = pDatabaseName.
    end constructor.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
    method public IAreaSet createAreas(pcnames as char extent ):
        define variable areas as IAreaSet no-undo.
        define variable i as integer no-undo.
        areas = getService():NewAreas().
        do i = 1 to extent(pcnames):
            areas:Add(newArea(pcnames[i])).
        end.    
        getService():GetAreas().
        return areas.
        catch e as Progress.Lang.Error :
        	handleError(e).	
        end catch.
    end method.
    
    method public IAreaSet getAreas():
        define variable areas as IAreaSet no-undo.
        define variable i as integer no-undo.
        areas = getService():GetAreas().
        return areas.
        catch e as Progress.Lang.Error :
            handleError(e). 
        end catch.
    end method.
    
    
    method public IAreaSet createAreas(pcnames as char):
        define variable i as integer no-undo.
        define variable areanames as character extent no-undo.
        extent(areanames) = num-entries(pcnames).
        do i = 1 to extent(areanames):
            areanames[i] = entry(i,pcnames).
        end.    
        return createAreas(areanames).
    end method.
    
	method public IArea createArea(cname as char  ):
	    define variable area as IArea no-undo.
	    define variable lok as logical no-undo.
	    area = newArea(cname).
		lok = getService():CreateArea(newArea(cname)).
		return area.
		catch e as Progress.Lang.Error :
        	handleError(e).
        end catch.
	end method.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	method protected void handleError(e as Progress.Lang.Error  ):
	    define variable errhandler as DataAdminErrorHandler no-undo.
	    
        if ShowErrors then 
	    do:
            errhandler = new DataAdminErrorHandler().
            errhandler:Error(e) .
	    end.
	    else undo, throw e.
	end method.

    method protected IArea newArea(cname as char  ):
        define variable area as IArea no-undo.
        define variable oextent as IExtent no-undo.
        area = getService():NewArea(cname).
        oextent = getService():NewExtent().
        area:Extents:Add(oextent).
        getService():CreateArea(area).
        return area.
    end method.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	method protected  DataAdminService createService(  ):
		return new DataAdminService(DatabaseName).
	end method.
	
    method protected  DataAdminService getService(  ):
        if not valid-object (Service) then
             Service = createService().
        return Service.
    end method.
    
    

    
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public AreaClient ( ):
        delete object Service no-error.
	end destructor.


end class.
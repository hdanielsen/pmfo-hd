 /*************************************************************/
  /* Copyright (c) 2011 by progress Software Corporation.      */
  /*                                                           */
  /* all rights reserved.  no part of this program or document */
  /* may be  reproduced in  any form  or by  any means without */
  /* permission in writing from progress Software Corporation. */
  /*************************************************************/
 /*------------------------------------------------------------------------
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :  
    Notes       : from adeuib
  ----------------------------------------------------------------------*/
using Pmfo.Tools.Gui.Model.Query from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.

interface Pmfo.Tools.Gui.Model.IModel: 
    define public property EntityName as character no-undo get. set.
    method public void FetchData(). 
    method public void FetchData(pcQuery as character). 
    method public void FetchData(pcQuery as character,pMonitor as IMonitor). 
    method public void FetchData(pcTable as char extent,pcQuery as character extent). 
    method public Query GetQuery(pcTable as char).
    method public Query GetQuery(i as int).
end interface.
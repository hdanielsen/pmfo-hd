/*************************************************************/
 /* Copyright (c) 2011 by progress Software Corporation.      */
 /*                                                           */
 /* all rights reserved.  no part of this program or document */
 /* may be  reproduced in  any form  or by  any means without */
 /* permission in writing from progress Software Corporation. */
 /*************************************************************/
/*------------------------------------------------------------------------
    File        : eventhandler.p
    Purpose     : event handler for widgets in classes 

    Syntax      :
    Description : The class that creates, defines or manages widgets must 
                  - implement _iwidgethandler 
                  - start this procedure persistent with the class as input.
                  - define persistent triggers to run the widgetEvent procedure in this
                    The event name is passed back to _iwidgethandler and can be 
                    anything really. (SELF can also be used to identify widget) 
                    example:
                      ON START-SEARCH OF mBrowse persistent 
                           run widgetEvent in WidgetHandler ("start-search"). 
                     

    Author(s)   : hdaniels
    Created     :  2011
    Notes       : 
  ----------------------------------------------------------------------*/

 
/* ***************************  Main Block  *************************** */

using Pmfo.Tools.Gui.View.IWidgetHandler from propath.

define input  parameter pEventhandler as IWidgetHandler no-undo.
procedure widgetEvent :
    define input parameter pcEvent as character no-undo.
    if valid-object(pEventHandler) then
        pEventhandler:WidgetEvent(pcEvent).
    catch e as Progress.Lang.Error :
            
    end catch. 
 end procedure.    
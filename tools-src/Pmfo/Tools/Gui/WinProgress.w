&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable dWidthDiff as decimal no-undo. 
define variable dHeightDiff as decimal no-undo.
define variable CloseOnEscape as logical no-undo.
define variable CloseOnSpace as logical no-undo.
define variable MinButtonRow as decimal no-undo.
define temp-table ttBar 
    field seq as integer    
    field bar as PRogress.Lang.object
    index idx as primary unique seq.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel 
     LABEL "Cancel Load" 
     SIZE 24 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnCancel AT ROW 26.24 COL 70 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 27.05 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Code Generator Load Monitor"
         HEIGHT             = 27.05
         WIDTH              = 95.4
         MAX-HEIGHT         = 80
         MAX-WIDTH          = 300
         VIRTUAL-HEIGHT     = 80
         VIRTUAL-WIDTH      = 300
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Code Generator Load Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
     
  IF CloseOnEscape = false and THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Code Generator Load Monitor */
DO:
  /* This event will close the window and terminate the procedure.  */
  run cancelTargets.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Code Generator Load Monitor */
DO:
    run resize. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel Load */
DO:
   run CancelTargets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN DestroyObject.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  C-Win:y = (Session:height-pixels - c-win:height-pixels) / 2.     
  C-Win:x = (Session:width-pixels - c-win:width-pixels) / 2.     
  
  RUN enable_UI.
  run createComponents.
/*  assign                                                                */
/*      dHeightDiff = c-win:height - edcode:height in frame default-frame.*/
/*      dWidthDiff = c-win:width - edcode:width in frame default-frame.   */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddProgressBar C-Win 
PROCEDURE AddProgressBar :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter pcText as character no-undo.
    define output parameter pBar as Pmfo.Tools.Gui.View.ProgressBar no-undo.
    define variable iSeq as integer no-undo.
    find last ttBar no-error.
    if avail ttBar then 
        iSeq = ttBar.seq + 1.
    else 
        iSeq = 1.
    create ttBar.
    pBar = new Pmfo.Tools.Gui.View.ProgressBar(frame default-frame:handle).
    ttBar.seq = iseq.
    ttBar.bar = pBar.
    pBar:Text = pcText.
    pBar:row = 2 + ((iSeq - 1) * (pBar:Height + 0.5)).
    pBar:View().
    c-win:height = pBar:row + pBar:Height + 2.
    run resize.
    MinButtonRow = BtnCancel:row.
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CancelTargets C-Win
procedure CancelTargets:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  for each ttBar:
      // this will cause CancelError to be thrown on next Increment() 
      // there need to be handlers for the error that stops the processing and also
      // closes this window 
      if valid-object(ttBar.bar) then
          cast(ttBar.bar,Pmfo.Tools.Gui.View.ProgressBar):Canceled = true.
  end.

end procedure.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateComponents C-Win 
PROCEDURE CreateComponents :
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DestroyObject C-Win 
PROCEDURE DestroyObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  for each ttBar:
      if valid-object(ttBar.bar) then 
          delete object ttBar.bar.
      delete ttBar.
  end.
  run disable_ui.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   c-Win:Move-To-Top().

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resize C-Win 
PROCEDURE resize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
      
    if c-win:width >= frame default-frame:width then
    do:
         assign
            frame default-frame:width = c-win:width no-error.
            for each ttBar:
               cast(ttBar.bar, Pmfo.Tools.Gui.View.ProgressBar):Resize(c-Win).
            end.  
            BtnCancel:col = c-win:width - BtnCancel:width no-error.
    end.
    else do:
        assign
            frame default-frame:scrollable = false.

            for each ttBar:
               cast(ttBar.bar, Pmfo.Tools.Gui.View.ProgressBar):Resize(c-Win).
            end. 
            BtnCancel:col = c-win:width - BtnCancel:width no-error.           
            frame default-frame:width = c-win:width no-error.
    end.
    
    if c-win:height >= frame default-frame:height   then
    do:
         frame default-frame:height = c-win:height no-error.  
         BtnCancel:row = c-win:height - BtnCancel:height no-error.
  //          edCode:height in frame default-frame = c-win:height - dheightDiff.

    end.
    else do:
        frame default-frame:scrollable = false.
    //        edCode:height in frame default-frame = c-win:height -  dheightDiff
        BtnCancel:row = max(MinButtonRow,c-win:height - BtnCancel:height) no-error.
        frame default-frame:height = c-win:height no-error.

    end.


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCloseOnEscape C-Win 
PROCEDURE SetCloseOnEscape :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   CloseOnEscape = true.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCloseOnSpace C-Win 
PROCEDURE SetCloseOnSpace :
/*------------------------------------------------------------------------------
 Purpose: Allow close on space bar
 Notes: The caller must wait-for close of hWindow or u1 of hWindow.
        did not find a way to make apply close work from any-key  
------------------------------------------------------------------------------*/
    CloseOnSpace = true.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setContent C-Win 
PROCEDURE setContent :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  define input  parameter pcTitle as character no-undo.
  define input  parameter pcLabel  as character no-undo. 
  define input  parameter pcName  as character no-undo.
  define input  parameter pcCode  as longchar no-undo.
  c-win:title = pcTitle.
  do with frame default-frame:
/*      edcode:screen-value = pccode.*/
/*      fiName:label = pcLabel.      */
/*      fiName:screen-value = pcname.*/
  end. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetHeight C-Win 
PROCEDURE SetHeight :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input parameter pheight as decimal no-undo.
   c-win:height = pheight.
   run resize. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWidth C-Win 
PROCEDURE SetWidth :
define input  parameter pwidth as decimal no-undo.
    c-win:width = pwidth.
    run resize. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


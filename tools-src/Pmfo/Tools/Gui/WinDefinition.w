&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File        : Pmfo/Tools/WinDefintion.w  
  Purpose     : Generate definition include
  Notes       : This exists to generate include for very large tables 
                like spec. Too big for the current WinDesign
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
define variable Generator as Pmfo.Tools.AppBuilder.DefinitionGenerator no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnGenerate cbDatabase cbTable fiInclude 
&Scoped-Define DISPLAYED-OBJECTS cbDatabase cbTable fiInclude 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGenerate 
     LABEL "Generate" 
     SIZE 17.4 BY 1.14.

DEFINE VARIABLE cbDatabase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE cbTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiInclude AS CHARACTER FORMAT "X(256)":U 
     LABEL "Include" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnGenerate AT ROW 1.62 COL 67.6 WIDGET-ID 6
     cbDatabase AT ROW 1.71 COL 11 COLON-ALIGNED WIDGET-ID 2
     cbTable AT ROW 3.33 COL 11 COLON-ALIGNED WIDGET-ID 4
     fiInclude AT ROW 5.05 COL 11 COLON-ALIGNED WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.4 BY 8.62 WIDGET-ID 100.


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
         TITLE              = "Generate Definitions"
         HEIGHT             = 8.71
         WIDTH              = 85.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 85.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 85.4
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
ON END-ERROR OF C-Win /* Generate Definitions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate Definitions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGenerate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGenerate C-Win
ON CHOOSE OF btnGenerate IN FRAME DEFAULT-FRAME /* Generate */
DO:
    session:set-wait-state("general").
    run generate.
    finally:
       session:set-wait-state("").
    end.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDatabase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDatabase C-Win
ON VALUE-CHANGED OF cbDatabase IN FRAME DEFAULT-FRAME /* Database */
DO:
   assign cbDatabase. 
   run dbChanged(cbDatabase).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTable C-Win
ON VALUE-CHANGED OF cbTable IN FRAME DEFAULT-FRAME /* Table */
DO:
   assign cbTable.
   run tableChanged.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiInclude
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiInclude C-Win
ON VALUE-CHANGED OF fiInclude IN FRAME DEFAULT-FRAME /* Include */
DO:
    assign fiInclude.
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dbChanged C-Win 
PROCEDURE dbChanged :
define input  parameter pDatabase as character no-undo.
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable Service as OpenEdge.DataAdmin.IDataAdminService no-undo.
    define variable TableSet as OpenEdge.DataAdmin.ITableSet no-undo.
    define variable iter as OpenEdge.DataAdmin.Lang.Collections.IIterator no-undo.
    define variable Tbl as OpenEdge.DataAdmin.ITable no-undo.
    session:set-wait-state("general").
    
    service = new OpenEdge.DataAdmin.DataAdminService(pDatabase).
    Tableset = Service:GetTables().
    iter = TableSet:Iterator().
    do with  frame {&frame-name}:
        cbTable:List-items = ?.
        do while iter:HasNext():
            Tbl = cast(Iter:Next(),OpenEdge.DataAdmin.ITable).
            cbTable:add-last(Tbl:name). 
        end.
        cbTable:screen-value = cbTable:entry(1).
        assign cbTable.
        run tableChanged.
    end.
    finally:
       session:set-wait-state("").
       delete object service. 
    end.    
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
  DISPLAY cbDatabase cbTable fiInclude 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnGenerate cbDatabase cbTable fiInclude 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generate C-Win 
PROCEDURE generate :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable cInclude as character no-undo.
    define variable lOk as logical no-undo.
    define variable cFullname as character no-undo.
    if num-entries(fiInclude,".") > 1 then
    do:
         message "Don't add file extension"
         view-as alert-box error.
         return.
    end.
    lok = true.
    cFullname = subst("&1/&2/&3.i",Generator:OutputDir,Generator:DefinitionDir,fiInclude).
    if search(cFullname) <> ?  then
    do:
       lok = false.
        message subst("&1 already exists!~n~nOverwrite?",cFullname)
        view-as alert-box question buttons yes-no update lok.
    
    end.  
    if lok then
    do:
        cInclude = Generator:Generate(cbDatabase,cbTable,fiInclude).
        message "File" cinclude "was generated"
        view-as alert-box information.
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize C-Win 
PROCEDURE Initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define variable i as integer no-undo.
   do with frame {&frame-name}:
       cbDatabase:list-items = ?.
       do i = 1 to num-dbs :
           cbDatabase:add-last(ldbname(i)).
       end.
       
       cbDatabase:screen-value = cbDatabase:entry(1).
       assign cbDatabase.
       run dbChanged(cbDatabase).  
       
   end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetGenerator C-Win 
PROCEDURE SetGenerator :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pGenerator as Pmfo.Tools.AppBuilder.DefinitionGenerator no-undo.
   Generator = pGenerator.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tableChanged C-Win 
PROCEDURE tableChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   fiInclude = Generator:GetIncludeName(cbTable).
   display fiInclude with frame default-frame.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


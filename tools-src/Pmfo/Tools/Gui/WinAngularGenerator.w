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
 
 {Pmfo/Repository/schema/entity.i}
 
define variable Generator      as Pmfo.Tools.AppBuilder.AngularModelGenerator no-undo.
define variable Starter        as handle no-undo.
define variable StartCallBack  as character no-undo.
define variable SessionManager as Pmfo.Core.Manager.ISessionManager no-undo.
define variable ServiceManager as Pmfo.Core.Manager.IServiceManager no-undo.
define variable CurrentBE      as Pmfo.Core.BusinessLogic.IBusinessEntity no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDir fiOutputDir 
&Scoped-Define DISPLAYED-OBJECTS fiOutputDir cbBe toGenerateFormBuilder ~
seFields 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDir 
     LABEL "Browse..." 
     SIZE 17.4 BY 1.14.

DEFINE BUTTON btnGenerate 
     LABEL "&Generate" 
     SIZE 17.4 BY 1.14.

DEFINE VARIABLE cbBe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Business Entity" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 82.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiOutputDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output Directory" 
     VIEW-AS FILL-IN 
     SIZE 82.8 BY 1 NO-UNDO.

DEFINE VARIABLE seFields AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 83 BY 24.29 NO-UNDO.

DEFINE VARIABLE toGenerateFormBuilder AS LOGICAL INITIAL no 
     LABEL "Generate Form Builder" 
     VIEW-AS TOGGLE-BOX
     SIZE 83 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDir AT ROW 1.71 COL 104.6 WIDGET-ID 10
     fiOutputDir AT ROW 1.81 COL 18.2 COLON-ALIGNED WIDGET-ID 8
     btnGenerate AT ROW 3.19 COL 104.6 WIDGET-ID 6
     cbBe AT ROW 3.33 COL 18.2 COLON-ALIGNED WIDGET-ID 4
     toGenerateFormBuilder AT ROW 4.71 COL 20 WIDGET-ID 18
     seFields AT ROW 6.71 COL 20 NO-LABEL WIDGET-ID 12
     "Select Search Fields" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 6 COL 20 WIDGET-ID 16
     "Fields:" VIEW-AS TEXT
          SIZE 6.6 BY .62 AT ROW 6.91 COL 13.4 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124 BY 30.81 WIDGET-ID 100.


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
         HEIGHT             = 30.81
         WIDTH              = 124
         MAX-HEIGHT         = 30.81
         MAX-WIDTH          = 146.4
         VIRTUAL-HEIGHT     = 30.81
         VIRTUAL-WIDTH      = 146.4
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
/* SETTINGS FOR BUTTON btnGenerate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbBe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST seFields IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX toGenerateFormBuilder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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


&Scoped-define SELF-NAME btnDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDir C-Win
ON CHOOSE OF btnDir IN FRAME DEFAULT-FRAME /* Browse... */
DO:
    define variable currentDir as character no-undo.
    file-info:file-name = ".".
    currentdir = file-info:full-pathname.
      
    system-dialog get-dir fiOutputDir
                   initial-dir currentdir
                  title "Select Angular Model Directory".
    display fiOutputDir with frame default-frame.         
    run RefreshUI.
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


&Scoped-define SELF-NAME cbBe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBe C-Win
ON VALUE-CHANGED OF cbBe IN FRAME DEFAULT-FRAME /* Business Entity */
DO:
   assign cbBe. 
   run beChanged. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOutputDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutputDir C-Win
ON VALUE-CHANGED OF fiOutputDir IN FRAME DEFAULT-FRAME /* Output Directory */
DO:
    assign fiOutputDir.
    run refreshUI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME seFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL seFields C-Win
ON VALUE-CHANGED OF seFields IN FRAME DEFAULT-FRAME
DO:
   assign seFields.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toGenerateFormBuilder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toGenerateFormBuilder C-Win
ON VALUE-CHANGED OF toGenerateFormBuilder IN FRAME DEFAULT-FRAME /* Generate Form Builder */
DO:
   assign toGenerateFormBuilder.  
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
  do:
    run initialize.
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beChanged C-Win 
PROCEDURE beChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 
    define variable hBuffer as handle no-undo.
    define variable hField  as handle no-undo.
    define variable iField   as integer no-undo.
    define variable cList as character no-undo.
    CurrentBE = ServiceManager:CreateBusinessEntity(cbBe).
    
    hBuffer = CurrentBE:Datasets[1]:get-buffer-handle(1).
    
    do with frame {&frame-name}:
        do iField = 1 to hBuffer:num-fields:
           hField = hBuffer:buffer-field(ifield).
           if hfield:serialize-hidden = false then 
           do: 
               cList = cList 
                     + (if cList = "" then "" else ",")
                     + (subst("&1,&2",hField:serialize-name,hField:name)).
           end.     
        end.
        if cList > "" then 
        do:
            seFields:list-item-pairs = cList.
            seFields:sensitive = true.
        end.
        else do:
            do iField = seFields:num-items to 1 by -1:
                seFields:delete(seFields:Entry(ifield)). 
            end.
            seFields:sensitive = false.
       end.      
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
  DISPLAY fiOutputDir cbBe toGenerateFormBuilder seFields 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnDir fiOutputDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillBe C-Win 
PROCEDURE fillBe :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter pbe    as Pmfo.Core.BusinessLogic.IBusinessEntity no-undo.
    define input parameter pCombo as handle no-undo.
    define variable hResourceTable as handle no-undo.
    define variable hDataset as handle no-undo.
    define variable hEntityTable as handle no-undo.
    pCombo:list-items = ?.
     
    pbe:GetData(output dataset-handle hDataset by-reference).
    hResourceTable = hdataset:Get-buffer-handle(1):table-handle.
    run fillBeStatic (input table-handle hResourceTable by-reference,pCombo).
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillBeStatic C-Win 
PROCEDURE fillBeStatic :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for ttEntity.
    define input parameter pCombo as handle no-undo.
    pCombo:list-items = ?.
    for each ttEntity where ttEntity.TypeName = "Pmfo.Core.BusinessLogic.IBusinessEntity"
                      by resource : 
       pCombo:add-last(ttentity.resource).
    end. 
    pcombo:screen-value = pCombo:entry (1).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generate C-Win 
PROCEDURE generate :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable cFile as character no-undo.
    define variable hBuffer as handle no-undo.
    hBuffer = CurrentBE:Datasets[1]:get-buffer-handle(1).
    cFile = Generator:Generate(fiOutputDir,hBuffer, seFields, toGenerateFormBuilder).
    message "File" cFile "was generated"
        view-as alert-box information.
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize C-Win 
PROCEDURE Initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    assign frame {&frame-name} fiOutputDir.
    if valid-handle(Starter) then
    do: 
        run value(StartCallBack) in Starter.
        SessionManager = cast(Ccs.Common.Application:SessionManager,Pmfo.Core.Manager.ISessionManager).
        ServiceManager = cast(Ccs.Common.Application:ServiceManager,Pmfo.Core.Manager.IServiceManager).
        run loadBusinessEntities.
    end.    
    if fiOutputDir > "" then display fiOutputDir with frame {&frame-name}.
    if not valid-object(Generator) then 
        Generator = new Pmfo.Tools.AppBuilder.AngularModelGenerator().
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadBusinessEntities C-Win 
PROCEDURE loadBusinessEntities :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable oResourceBe as Pmfo.Core.BusinessLogic.IBusinessEntity no-undo.
    oResourceBe = ServiceManager:CreateBusinessEntity("resources").
    run fillBe(oResourceBE, cbBe:handle in frame {&frame-name}).
    cbBe:sensitive in frame {&frame-name} = cbBe:list-items in frame {&frame-name} <> ?.
    run refreshUI.
    assign frame {&frame-name} cbBe.
    assign frame {&frame-name} toGenerateFormBuilder.  
    run beChanged. 
    catch e as Progress.Lang.Error :
       message "Failed to load Resource Entity = Error: " skip
                e:GetMessage(1)
       view-as alert-box error.
       
            
    end catch. 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshUI C-Win 
PROCEDURE refreshUI :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
     file-info:file-name = fiOutputdir. 
     if file-info:file-type matches "*D*" then
         btnGenerate:sensitive in frame {&frame-name} = cbBe:sensitive in frame {&frame-name}. 
     else btnGenerate:sensitive in frame {&frame-name} = false.
     toGenerateFormBuilder:sensitive in frame {&frame-name} = btnGenerate:sensitive in frame {&frame-name} .
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetGenerator C-Win 
PROCEDURE SetGenerator :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pGenerator as Pmfo.Tools.AppBuilder.AngularModelGenerator no-undo.
   Generator = pGenerator.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetOutputDir C-Win 
PROCEDURE SetOutputDir :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  define input  parameter pcDir as character no-undo.
 
  fiOutputDir = pcDir.
   display fiOutputDir with frame {&frame-name}.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetStartLibInfo C-Win 
PROCEDURE SetStartLibInfo :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter phHandle as handle no-undo.
   define input  parameter pcStartLib as character no-undo.
   Starter = phHandle.
   StartCallBack = pcStartLib.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


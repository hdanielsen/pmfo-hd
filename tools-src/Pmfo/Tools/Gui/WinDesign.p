 /*------------------------------------------------------------------------
    File        : WinDesign.p 
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Jan 2019
    Notes       : UI for code generation of defintions, data sources and 
                  business entities 
                  To be run from in designed application propath and connections
                  and to be extended using the 
                APIS to plugin application specific extensions
                - SetCodeConverter(CodeConverter) 
                  optional CodeConverter - typically sub classed to deal with
                  commonly used code patterns in SDO and DLP code  
                - SetCodeTableDataWindowName(char)
                  optional window to open from btnShowData to show data from
                  code table   - run value(name)
                - SetCodeTableView(ICodeTableview) 
                  optional view to display code table fields  
                   dynamic-new 
                     - implement Pmfo.Tools.Gui.View.ICodeTableView 
                     - constructor(parent frame)  
                   lower left frame - must fit below datasource and left of 
                   errors. (May need improvement to mange wider)  
                - SetNameUtil(INameUtil) 
                  - optional utility naming utility - default to use 
                    Pmfo.Tools.Util.NameUtil if not set.
                    Allows subclass that deals with naming of classes, defintion
                    include, temp-tables and temp-table and field serialize-names
                    from database defintions for the specific application   
                - SetOutputDirectory(char) 
                  - optional - set dir name to avoid having to type it in the ui 
                - SetSdoDirectory(char) 
                  - optional - set dir name to avoid having to type it in the ui 
                    will retrieve all resources on initialize if set   
                - SetTheme(ITheme) 
                  - optional theme - will use Pmfo.Tools.Util.DefaultTheme if not 
                    set  
                The frame is 320 chars wide, which is max width that can fit 
                static widgets - BUG - OCTA-11685 
                If it need to be wider it must be done dynamically.    
  ----------------------------------------------------------------------*/
  
using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.Manager.StartupManager from propath.
using Pmfo.Tools.AppBuilder.CodeConverter from propath.
using Pmfo.Tools.AppBuilder.CodeGenerator from propath.
using Pmfo.Tools.AppBuilder.ErrorTracker from propath.
using Pmfo.Tools.AppBuilder.ErrorTrackerEnum from propath.
using Pmfo.Tools.Gui.Model.BusinessEntityModel from propath.
using Pmfo.Tools.Gui.Model.CodeModel from propath.
using Pmfo.Tools.Gui.Model.DataSourceModel from propath.
using Pmfo.Tools.Gui.Model.DlpModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
using Pmfo.Tools.Gui.Model.FunctionModel from propath.
using Pmfo.Tools.Gui.Model.IResourceModel from propath.
using Pmfo.Tools.Gui.Model.ResourceModel from propath.
using Pmfo.Tools.Gui.Model.SdoModel from propath.
using Pmfo.Tools.Gui.View.FieldBrowse from propath.
using Pmfo.Tools.Gui.View.ICodeTableView from propath.
using Pmfo.Tools.Gui.View.ResourceBrowse from propath.
using Pmfo.Tools.Util.DefaultTheme from propath.
using Pmfo.Tools.Util.ITheme from propath.
using Pmfo.Util.Array from propath.
using Progress.Lang.Error from propath.
using Pmfo.Tools.Gui.View.IView from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Tools.Util.ColorManager from propath.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.
using Pmfo.Tools.Gui.Model.Monitor from propath.
using Pmfo.Tools.Gui.Model.CancelError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
 
create widget-pool.

/* ***************************  Definitions  ************************** */
define variable resourceBrowse    as ResourceBrowse no-undo.
define variable fieldBrowse       as FieldBrowse no-undo.

// optional view class name(with frame) to display code table - positioned under fields of this window
define variable CodeTableView      as ICodeTableView no-undo. 
define variable CodeConverter      as CodeConverter no-undo.
define variable Theme              as ITheme no-undo.
define variable CodeGeneratorClass as Progress.Lang.Class no-undo.
define variable Monitor            as IMonitor no-undo.
define variable resourceModel      as IResourceModel  no-undo.
define variable nameService        as INameService    no-undo.
// optionalmodel for CodeTable - use SetCodeTableModel from application
define variable codeTableModel     as ICodeTableModel no-undo. 
define variable cTitle             as character init "Generate Code" no-undo.
 

// optional filter code table - positioned ro the right of code table toggle    
// Use SetCodeTableSearchWindowName to set from application 

define variable CodeTableSearchWindowName as character no-undo.
define variable CodeTableSearchWindow     as handle    no-undo.

/* optional window to open from Show Data button for code table
 -  must have procedures:
    - OpenQuery - input parameter table for ttdatafield (),
    - initialize - just in case - no required behavior
    - moveToTop  - window:move=to=top()
 use SetCodeTableDataWindowName to set proc name from application */   
define variable CodeTableDataWindowName  as character no-undo.
define variable CodeTableDataWindow      as handle no-undo.


define variable sdoProcWin as handle no-undo.
define variable sdoFuncWin as handle no-undo.
define variable dlpProcWin as handle no-undo.
define variable dlpFuncWin as handle no-undo.
define variable dataSourceMethodWin as handle no-undo.
define variable businessEntityMethodWin as handle no-undo.
define variable superLib as character no-undo.


define variable initialized    as logical         no-undo.
define variable DefinitionDir  as character init "schema"  no-undo.


 //{Util/filedata.i by-reference}
 
{Pmfo/Tools/AppBuilder/resourcedata.i reference-only}
{Pmfo/Tools/AppBuilder/fieldData.i }

/* ************************  Function Prototypes ********************** */

function AddError returns ErrorTracker 
    (pErrorTracker as ErrorTracker,
     pEnum as ErrorTrackerEnum,
     piParam as char extent) forward.

function AddTTLengthError returns ErrorTracker 
    (pErrorTracker as ErrorTracker,
     pTTname as char) forward.

function AskEmergency returns logical 
    (perror as Error,
     plMany  as log) forward.

function CreateBusinessEntity returns BusinessEntityModel 
    (input table resourceData) forward.

function CreateDataSource returns DataSourceModel 
    (input table resourceData) forward.

function GetFields returns ISet 
    (input table resourceDAta) forward.

function getFullDirectoryPath returns character 
    (pcdir as character) forward.

function GetSdoDirectory returns character 
    (  ) forward.

function GetSelectedCode returns longchar 
  (pcname as char,
   pMethods as ICollection,
   output pdatatype as char) forward.

function GetServiceManager returns IServiceManager 
    (  ) forward.

function RemoveError returns ErrorTracker 
    (pErrorTracker as ErrorTracker,
     pEnum as ErrorTrackerEnum,
     piParam as char extent) forward.

function RemoveTTLengthError returns ErrorTracker 
    (pErrorTracker as ErrorTracker,
     pTTname as character) forward.

function GetErrorMessages returns LONGCHAR 
    (perror as ErrorTracker) forward.

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define button btnOutDir 
     label "Browse..." 
     size 15 by 1.

define button btnOutSourceDir 
     label "Browse..." 
     size 15 by 1.

define button btnSdoDir 
     label "Browse..." 
     size 15 by 1.
     
define button btnGenerate 
     label "Generate Code" 
     size 19 by 1.

define button btnRefreshService 
     label "Refresh Service" 
     size 19 by 1.

define button btnShowData 
     label "Show Code Table Data..." 
     size 25 by 1.

define button btnSearchCode 
     label "Search Code Table..." 
     size 23 by 1.

define variable fiOutput as character format "X(256)":U 
     label "Application Output Directory" 
     view-as fill-in 
     size 92 by 1 no-undo.

define variable fiSourceOutput as character format "X(256)":U 
     label "DataLayer Output Directory" 
     view-as fill-in 
     size 92 by 1 no-undo.

define variable toSDOs as logical  
     init true
     label "With SDOs" 
     view-as toggle-box 
     size 18 by 1 no-undo.

define variable toNoSDOs as logical  
     init true
     label "Tables without SDOs" 
     view-as toggle-box 
     size 25 by 1 no-undo.

define variable toDataFields as logical  
     init true
     label "Code Table" 
     view-as toggle-box 
     size 18 by 1 no-undo.

define variable toIncludes as logical  
     label "Definition Includes" 
     view-as toggle-box 
     size 23 by 1 no-undo.
     
define variable toDataSources as logical  
     label "Data Sources" 
     view-as toggle-box 
     size 20 by 1 no-undo.

define variable toBusinessEntities as logical  
     label "Business Entities" 
     view-as toggle-box 
     size 20 by 1 no-undo.
 
define variable toOverwriteBE as logical  
     label "Overwrite" 
     view-as toggle-box 
     size 14 by 1 no-undo.
     
define variable toOverwriteInclude as logical  
     label "Overwrite" 
     view-as toggle-box 
     size 14 by 1 no-undo.
     
define variable toOverwriteDS as logical  
     label "Overwrite" 
     view-as toggle-box 
     size 14 by 1 no-undo.
         
     
define variable fiSDO as character format "X(256)":U 
     label "SDO Directory" 
     view-as fill-in 
     size 92 by 1 no-undo.

define variable fiClassName as character format "X(256)":U 
     label "Base Name" 
     view-as fill-in 
     size 35 by 1 no-undo.
     
define variable fiBusinessEntity as character format "X(256)":U 
     label "Business Entity" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable fiEntityName as character format "X(256)":U 
     label "Entity" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable fiIncludeFile as character format "X(256)":U 
     label "Definition File" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable firectGenerateTitle as character format "X(256)":U 
     init "Generate"
     view-as text 
     size 9 by 0.8 no-undo.
     
define variable firectFilterTitle as character format "X(256)":U 
     init "Filter"
     view-as text 
     size 5 by 0.8 no-undo.

define variable fiSdoProcLabel as character format "X(256)":U 
     init "SDO Procedures"
     view-as text 
     size 16 by 0.8 no-undo.

define variable fiSdoFuncLabel as character format "X(256)":U 
     init "SDO Functions"
     view-as text 
     size 16 by 0.8 no-undo.

define variable fiDlpProcLabel as character format "X(256)":U 
     init "DLP Procedures"
     view-as text 
     size 16 by 0.8 no-undo.

define variable fiDlpFuncLabel as character format "X(256)":U 
     init "DLP Functions"
     view-as text 
     size 16 by 0.8 no-undo.

define variable fiErrorLabel as character format "X(256)":U 
     init "Errors or warnings"
     view-as text 
     size 50 by 0.8 no-undo.

define variable fiQueryLabel as character format "X(256)":U 
     init "Query"
     view-as text 
     size 50 by 0.8 no-undo.

define variable fiBeMethodLabel as character format "X(256)":U 
     init "Business Entity Methods"
     view-as text 
     size 26 by 0.8 no-undo.

define variable fiDSMethodLabel as character format "X(256)":U 
     init "Data Source Methods"
     view-as text 
     size 26 by 0.8 no-undo.

define variable fiDataSource as character format "X(256)":U 
     label "Data Source" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable fiResource as character format "X(256)":U 
     label "Resource" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable fiTempTableName as character format "X(255)":U 
     label "Temp-Table Name" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable fiTableName as character format "X(255)":U 
     label "Table Name" 
     view-as fill-in 
     size 35 by 1 no-undo.

define variable toNotInUse as logical 
     label "Not In Use" 
     view-as toggle-box 
     size 12 by 1 no-undo.

define variable dlpFunctions as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.

define variable dlpProcedures as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.

define variable sdoFunctions as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.

define variable sdoProcedures as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.

define variable dataSourceMethods as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.

define variable businessEntityMethods as character 
     view-as selection-list single scrollbar-vertical 
     size 35 by 9 no-undo.
     
define rectangle rGenerate 
    edge-pixels 1 graphic-edge  no-fill   rounded 
    size 90 by 3.8. 

define rectangle rFilter 
    edge-pixels 1 graphic-edge  no-fill   rounded 
    size 50 by 3.8. 

define variable raGenerateAllorOne as logical 
     view-as radio-set vertical
     radio-buttons 
          "Selected Resource", false,
          "All", true
     size 22 by 2 no-undo.

/* Definitions of the field level widgets                               */
define variable edErrors as longchar 
     view-as editor scrollbar-vertical large 
     size 73 by 9 no-undo.

/* Definitions of the field level widgets                               */
define variable edQuery as character 
     view-as editor scrollbar-vertical large
     size 149.3 by 3.5 no-undo.

/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     fiOutput at row 2 col 29 colon-aligned  
     btnOutDir at row 2 col 124
     fiSourceOutput at row 3.2 col 29 colon-aligned  
     btnOutSourceDir at row 3.2 col 124
     fiSDO at row 4.4 col 29 colon-aligned  
     btnSdoDir at row 4.4  col 124
     
     firectFilterTitle at row 1.4 col 178 colon-aligned no-label
     rFilter at row 1.7 col 177.2
     toSDOs at row 2.2 col 178 colon-aligned no-label
     toNoSDOs at row 3.2 col 178 colon-aligned no-label
     toDataFields at row 4.2 col 178 colon-aligned no-label
     btnSearchCode at row 4.2 col 200 colon-aligned no-label
     
     
     fiRectGeneratetitle at row 1.4 col 230 colon-aligned no-label
     rGenerate at row 1.7 col 229.2 
     toIncludes at row 2.2 col 230 colon-aligned
     toOverwriteInclude at row 2.2 col 253 colon-aligned
     
     toDataSources at row 3.2  col 230 colon-aligned
     toOverwriteDS at row 3.2 col 253 colon-aligned
     
     toBusinessEntities  at row 4.2  col 230 colon-aligned
     toOverwriteBe       at row 4.2  col 253 colon-aligned  
     raGenerateAllorOne  at row 2.2  col 272 colon-aligned no-label
     btnGenerate         at row 2.2  col 298
     btnRefreshService   at row 4.2  col 298
    
     btnShowData      at row 22.45 col 292.5
     fiClassName      at row 23.5  col 19 colon-aligned
     fiResource       at row 24.7  col 19 colon-aligned  
     fiEntityName     at row 25.9  col 19 colon-aligned  
     fiBusinessEntity at row 27.1  col 19 colon-aligned 
     fiIncludeFile    at row 28.3  col 19 colon-aligned 
     fiTempTableName  at row 29.5  col 19 colon-aligned
     fiDataSource     at row 30.7  col 19 colon-aligned  
     
     fiBeMethodLabel       at row 22.8  col 65 no-label   
     businessEntityMethods at row 23.5  col 65 no-label 
     
     fiErrorLabel          at row 33   col 65 no-label 
     edErrors              at row 33.7 col 65 no-label
     fiQueryLabel          at row 43   col 65 no-label 
     edQuery               at row 43.7 col 65 no-label
     
     fiDSMethodLabel       at row 22.8 col 103 no-label
     dataSourceMethods     at row 23.5 col 103 no-label  
     
     fiDlpProcLabel        at row 22.8 col 141 no-label
     dlpProcedures         at row 23.5 col 141 no-label  
     
     fiSdoProcLabel        at row 22.8 col 179 no-label
     sdoProcedures         at row 23.5 col 179 no-label  
     
     fiDlpFuncLabel        at row 33   col 141 no-label
     dlpFunctions          at row 33.7 col 141 no-label  
     
     fiSdoFuncLabel        at row 33   col 179 no-label
     sdoFunctions          at row 33.7 col 179 no-label 
     
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 320 by 47  widget-id 100.

/* *************************  Create Window  ************************** */

  create window C-Win assign
         hidden             = yes
         title              = cTitle
         height             = 47 
         width              = 320
         max-height         = 47
         max-width          = 320
         virtual-height     = 47
         virtual-width      = 320
         resize             = yes
         scroll-bars        = no
         status-area        = no
         bgcolor            = ?
         fgcolor            = ?
         keep-frame-z-order = yes
         three-d            = yes
         message-area       = no
         sensitive          = yes.

C-Win:hidden = yes.


on end-error of C-Win /* <insert window title> */
or endkey of C-Win anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

on window-close of C-Win /* <insert window title> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

on value-changed of toNoSDOs in frame DEFAULT-FRAME 
do:
    assign toNOSdos.
    run openQuery.
end.

on value-changed of toSDOs in frame DEFAULT-FRAME 
do:
    assign toSDos.
    run openQuery.
end.

on value-changed of toDataFields in frame DEFAULT-FRAME 
do:
    assign toDataFields.
    run RefreshUI.
    run openQuery.
end.

on value-changed of toBusinessEntities in frame DEFAULT-FRAME 
do:
    assign toBusinessEntities.
    run RefreshUI.
end.

on value-changed of toDataSources in frame DEFAULT-FRAME 
do:
    assign toDataSources.
    run RefreshUI.
end.

on value-changed of toIncludes in frame DEFAULT-FRAME 
do:
    assign toIncludes.
    run RefreshUI.
end.

on value-changed of toOverwriteInclude in frame DEFAULT-FRAME 
do:
    assign toOverwriteInclude.
end.

on value-changed of toOverwriteDS in frame DEFAULT-FRAME 
do:
    assign toOverwriteDs.
end.

on value-changed of toOverwriteBE in frame DEFAULT-FRAME 
do:
    assign toOverwriteInclude.
end.

on value-changed of raGenerateAllorOne in frame DEFAULT-FRAME 
do:
    assign frame DEFAULT-FRAME raGenerateAllorOne.     
end.

on value-changed of fiClassName in frame DEFAULT-FRAME 
do:
   assign fiClassName. 
   run resourceChanged. 
end.

on value-changed of fiResource in frame DEFAULT-FRAME 
do:
   assign fiResource. 
   run resourceChanged. 
end.

on value-changed of fiEntityName in frame DEFAULT-FRAME 
do:
   assign fiEntityName. 
   run resourceChanged. 
end.

on value-changed of fiBusinessEntity in frame DEFAULT-FRAME 
do:
   assign fiBusinessEntity. 
   run resourceChanged. 
end.

on value-changed of fiIncludeFile in frame DEFAULT-FRAME 
do:
   assign fiIncludeFile. 
   run resourceChanged. 
end.


on value-changed of fiTempTableName in frame DEFAULT-FRAME 
do:
   assign fiTempTableName. 
   run resourceChanged. 
end.

on value-changed of fiDataSource in frame DEFAULT-FRAME 
do:
   assign fiDataSource. 
   run resourceChanged. 
end.

on choose of btnOutDir in frame DEFAULT-FRAME 
do:
    system-dialog get-dir fiOutput
                  title "Select Output Directory".
    display fiOutput with frame default-frame.                
end.

on choose of btnOutSourceDir in frame DEFAULT-FRAME 
do:
    system-dialog get-dir fiSourceOutput
                  title "Select Output Directory".
    display fiSourceOutput with frame default-frame.                
end.

on choose of btnGenerate in frame DEFAULT-FRAME 
do:
    run generateCode. 
end.

on choose of btnRefreshService in frame DEFAULT-FRAME 
do:
    run refreshService. 
end.

on choose of btnShowData in frame DEFAULT-FRAME 
do: 
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run ShowCodeTableData(input table-handle hTable by-reference).
end.

on choose of btnSearchCode in frame DEFAULT-FRAME 
do: 
    run ShowCodeTableFilter.
end.

on choose of btnSdoDir in frame DEFAULT-FRAME 
do:
    define variable currentDir as character no-undo.
    file-info:file-name = ".".
    currentdir = file-info:full-pathname.
      
    system-dialog get-dir fiSDO
                   initial-dir currentdir
                  title "Select SDO Directory".
    display fiSDO with frame default-frame.         
    run SdoDirChanged(fiSDO).   
end.

on return of fiOutput in frame DEFAULT-FRAME /* Output Directory */
do:
   assign fiOutput. 
   run OutputDirchanged(fiOutput).   
end.

on return of fiSDO in frame DEFAULT-FRAME /* SDO Directory */
do:
   assign fiSDO. 
   run SdoDirChanged(fiSDO).   
end.

on default-action of dataSourceMethods in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run DataSourceMethodSelected(input table-handle hTable by-reference).
end.

on default-action of businessEntityMethods in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run BusinessEntityMethodSelected(input table-handle hTable by-reference).
end.

on default-action of dlpFunctions in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run DlpFunctionSelected(input table-handle hTable by-reference).
end.

on default-action of dlpProcedures in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run DlpProcedureSelected(input table-handle hTable by-reference).
end.

on default-action of sdoFunctions in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run SdofunctionSelected(input table-handle hTable by-reference).
end.

on default-action of sdoProcedures in frame DEFAULT-FRAME
do:
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run SdoProcedureSelected(input table-handle hTable by-reference).
end.

on tab of fiDataSource in frame DEFAULT-FRAME /* Fill 2 */
do:
   if valid-object(CodeTableView) then
   do:
       CodeTableview:SetFocus().
       return no-apply.
   end.        
end.

/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = c-Win 
       THIS-PROCEDURE:CURRENT-WINDOW = C-win.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run destroyObject.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  run CreateComponents.     
  if not this-procedure:persistent then do:
      run Initialize.
      wait-for close of this-procedure.
  end.  
end.

/* **********************  Internal Procedures  *********************** */
procedure CreateComponents :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   resourceModel = new ResourceModel().
   // browsers do not become visible if this is done at initialize. 
   run CreateResourceBrowse(resourceModel).
   run CreateFieldBrowse.
   resourceModel:DefinitionDir = DefinitionDir. 
   run enable_UI.
end procedure.


procedure CreateFieldBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    fieldBrowse = new FieldBrowse(frame DEFAULT-FRAME:handle).
    fieldBrowse:bind(table fieldData bind).   
    fieldBrowse:Init().
    fieldBrowse:row = sdoProcedures:row in frame default-frame.
    fieldBrowse:col = 221.2. //231.2.
    fieldBrowse:View().  
    fieldBrowse:Enable().  
    
    fieldBrowse:FieldValueChanged:subscribe("UpdateField") . 

end procedure.

procedure CreateCodeTableView :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
     if valid-object(CodeTableView) then 
     do:
         CodeTableView:Create(frame DEFAULT-FRAME:handle). 
         CodeTableView:Init().
         CodeTableView:row = fiDataSource:row in frame default-frame + 1.2 .
         CodeTableView:View().
         CodeTableModel:PositionChanged:subscribe(CodeTableView:DisplayCodeTable).  
     end.    
end procedure.



procedure CreateResourceBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter presourceModel as IResourceModel no-undo. 
   
    define variable hTbl as handle no-undo.
    resourceBrowse = new ResourceBrowse(frame DEFAULT-FRAME:handle).
    resourceBrowse:DefinitionDir = DefinitionDir. // for color coding in rowdisplay 
    hTbl = presourceModel:GetTable("resourceData").
    resourceBrowse:bind(table-handle htbl bind).   
    resourceBrowse:Init().
    resourceBrowse:row = 6.
    resourceBrowse:col = 3.
    resourceBrowse:View().  
    resourceBrowse:Enable().  
    resourceBrowse:ValueChanged:subscribe("DisplayResource") . 
end procedure.


procedure destroyObject :
    if valid-object(CodeTableView) then 
        delete object CodeTableView.
    if valid-object(Theme) then 
        delete object Theme.
    if valid-object(Monitor) then 
        delete object Monitor.
    if valid-object(resourceBrowse) then
        delete object(resourceBrowse).
    if valid-object(fieldBrowse) then
        delete object(fieldBrowse).
    if valid-object(ResourceModel) then
        delete object(ResourceModel).
    if valid-object(NameService) then
        delete object(NameService).    
    if valid-object(CodeConverter) then
        delete object(CodeConverter).    
    if valid-handle(CodeTableDataWindow)  then
    do:
        apply "close" to CodeTableDataWindow.
    end. 
    if valid-handle(CodeTableSearchWindow)  then
    do:
        apply "close" to CodeTableSearchWindow.
    end.    
    if valid-handle(sdoProcWin)  then
    do:
        apply "close" to sdoProcWin.
    end.    
    if valid-handle(sdoFuncWin)  then
    do:
        apply "close" to sdoFuncWin.
    end.    
    if valid-handle(dlpProcWin)  then
    do:
        apply "close" to dlpProcWin.
    end.    
    if valid-handle(dataSourceMethodWin)  then
    do:
        apply "close" to dataSourceMethodWin.
    end.    
    if valid-handle(businessEntityMethodWin)  then
    do:
        apply "close" to businessEntityMethodWin.
    end.    
    run disable_UI.    
end.


procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
  then delete widget C-Win.
  if this-procedure:persistent then delete procedure this-procedure.
end procedure.

procedure fetchResources:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   resourceModel:FetchData(fiSDO, Monitor).
   run OpenQuery.

end procedure.

procedure SensitizeResourceWidgets:
    define input parameter table for resourceData.
    if avail resourceData then 
    do with frame default-frame:
        assign
            btnGenerate:sensitive = true
/*            btnGenerate:sensitive = (toBusinessEntities or toDataSources or toIncludes)                                                                         */
/*                                    and (avail resourceData                                                                                                     */
/*                                         and (resourceData.NotFromData = false or resourceData.onlyInclude)                                                     */
/*                                         and (resourceData.definedinDb or resourceData.tablename = "" or resourceData.tablename = codeTableModel:CodeTableName))*/
            toIncludes:checked   = if resourceData.onlyInclude then false else toIncludes                              
            toIncludes:sensitive = if resourceData.onlyInclude then false else true                              
            fiClassName:sensitive      = avail resourceData
            fiResource:sensitive       = avail resourceData
            fiEntityName:sensitive     = avail resourceData
            fiBusinessEntity:sensitive = avail resourceData
            fiIncludeFile:sensitive    = avail resourceData
            fiTempTableName:sensitive  = avail resourceData and resourceData.onlyInclude = false
            fiDataSource:sensitive     = avail resourceData
            btnShowData:sensitive      = avail resourceData and resourceData.codeTableName > "" and CodeTableDataWindowName > "".
            . 
    end. 
end.    

procedure RefreshUI :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable hTable as handle no-undo.
    btnGenerate:sensitive in frame default-frame = toBusinessEntities or toDataSources or toIncludes. 
    btnRefreshService:sensitive in frame default-frame = true.
    toOverwriteInclude:sensitive in frame default-frame = toIncludes.
    toOverwriteDS:sensitive in frame default-frame = toDataSources.
    toOverwriteBE:sensitive in frame default-frame = toBusinessEntities.
    btnSearchCode:sensitive in frame default-frame = toDataFields.
    
    // not very elegant... 
    if btnGenerate:sensitive in frame default-frame then
    do:
        ResourceBrowse:GetTable(output table-handle hTable by-reference).
        run SensitizeResourceWidgets(input table-handle hTable by-reference).            
    end.
     
    if not toDataFields then
        run CloseCodeTableFilter.
    
    if not toIncludes then 
    do with frame default-frame:
       toOverwriteInclude = false.
       display toOverwriteInclude.
    end.    
    if not toDataSources then 
    do with frame default-frame:
       toOverwriteDS = false.
       display toOverwriteDS.
    end.    
    if not toBusinessEntities then 
    do with frame default-frame:
       toOverwriteBE = false.
       display toOverwriteBE.
    end.    
end procedure.

procedure ResourceChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable hTable as handle no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    run UpdateResource(input table-handle hTable by-reference). 
end procedure.

procedure SetCodeTableSearchWindowName:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pName as character no-undo.
   CodeTableSearchWindowName = pName.
end procedure.

procedure UpdateField :
    define input parameter table for fieldData.
    define variable ofields as ISet no-undo.
    define variable hTable  as handle no-undo.
    define variable oIter   as IIterator no-undo.
    define variable oField  as FieldModel no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    ofields = GetFields(input table-handle hTable by-reference).
    if valid-object(oFields) then do:
        oIter = ofields:Iterator().
        do while oIter:HasNext():
            oField = cast(OIter:Next(),FieldModel).
            if oField:name = fieldData.fieldName then 
            do:
                oField:serializeName = fieldData.serializeName.
                oField:IsExcluded = fieldData.isExcluded.
                return.
            end.    
        end.    
    end.    
end.    

procedure UpdateMethod :
    
     
    define variable ofields as ISet no-undo.
    define variable hTable  as handle no-undo.
    define variable oIter   as IIterator no-undo.
    define variable oField  as FieldModel no-undo.
    ResourceBrowse:GetTable(output table-handle hTable by-reference).
    ofields = GetFields(input table-handle hTable by-reference).
    if valid-object(oFields) then do:
        oIter = ofields:Iterator().
        do while oIter:HasNext():
            oField = cast(OIter:Next(),FieldModel).
            if oField:name = fieldData.fieldName then 
            do:
                oField:serializeName = fieldData.serializeName.
                oField:IsExcluded = fieldData.isExcluded.
                return.
            end.    
        end.    
    end.    
end.  



procedure UpdateResource :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
    define variable lTTerror as logical no-undo.
    define variable cTtName  as character no-undo.
   
    cTtname = resourceData.tempTableName.
    if compare(fiClassName,"<>",resourceData.className,"case-sensitive") then
    do:
         resourceData.className = fiClassName.
         resourceModel:AfterRow(table resourceData by-reference).
         run DisplayResourcefields(table resourceData by-reference).   
    end.
    if compare(fiResource,"<>",resourceData.resourceName,"case-sensitive") then
    do:
         resourceData.resourceName = fiResource.          
    end.
    if compare(fiEntityName,"<>",resourceData.entityName,"case-sensitive") then
    do:
         resourceData.entityName = fiEntityName.          
    end.
    if compare(fiBusinessEntity,"<>",resourceData.businessEntity,"case-sensitive") then
    do:
         resourceData.businessEntity = fiBusinessEntity.          
    end.
    if compare(fiIncludeFile,"<>",resourceData.includefile,"case-sensitive") then
    do:
         resourceData.includefile = fiIncludeFile.          
    end.
    if compare(fiTempTableName,"<>",resourceData.tempTableName,"case-sensitive") then
    do:
         resourceData.tempTableName = fiTempTableName.          
    end.
    if compare(fiDataSource,"<>",resourceData.dataSource,"case-sensitive") then
    do:
         resourceData.dataSource = fiDataSource.          
    end.
    
    if cTTName <> resourceData.temptablename then 
    do:
        if length(cTTName) > 32 then 
            resourceData.Error = RemoveTTLengthError(cast(resourceData.Error,ErrorTracker), cTtname). 
        
        if length(resourceData.tempTableName) > 32 then 
            resourceData.Error = AddTTLengthError(cast(resourceData.Error,ErrorTracker), resourceData.temptablename). 
        run DisplayResourcefields(table resourceData by-reference).
    end.
    resourceBrowse:Display().
end procedure.

// display the resource data in the ui (the selectionlists are disaplyed elsewhere )
procedure DisplayResourceFields :
    define input parameter table for resourceData.
    define variable oDataSource as DataSourceModel no-undo.
    do with frame default-frame:
        if avail resourceData then 
        do:
            if valid-object(resourceData.DataSourceModel) then 
               oDataSource = cast(resourceData.DataSourceModel,DataSourceModel).
                
            // NOTE do not assign screen-value !!!
            // it will fire valuechanged where this is fired from 
            // and record will not be avail and data becomes blank
            assign
                fiClassname      = resourceData.Classname
                fiResource       = resourceData.resourceName
                fiEntityName     = resourceData.entityName
                fiBusinessEntity = resourceData.businessEntity
                fiIncludeFile    = resourceData.includefile
                fiTempTableName  = resourceData.tempTableName
                fiDataSource     = resourceData.dataSource
                edQuery          = replace(replace(if valid-object(oDataSource) then oDataSource:baseQuery else "",
                                                   ", ",
                                                   "," + StringConstant:LF + "    "),
                                           " by ",
                                           StringConstant:LF + "        by ").
             if valid-object(resourceData.error) then
             do:   
                 edErrors         = GetErrorMessages(cast(resourceData.error,ErrorTracker)). 
                 edErrors:bgcolor =  Theme:ErrorBg.
                 edErrors:fgcolor =  Theme:ErrorFg.
             end.
             else 
             if resourceData.noCodes 
             or resourcedata.NotInUse 
             or (resourcedata.tableName > "" and resourcedata.Tablename <> codeTableModel:CodeTableName and resourceData.definedinDb = false) then 
             do:
                  edErrors = "".
                  if resourceData.noCodes then
                 
                      edErrors = subst("There is no data in the '&1' table for &2 &3.",
                                       codeTableModel:CodeTableName, 
                                       if codeTableModel:CodeTableKeyType = "integer" then codeTableModel:CodeTableKeyLabel else codeTableModel:CodeTableNameLabel,
                                       if codeTableModel:CodeTableKeyType = "integer" then string(resourcedata.codeTableKey) else quoter(resourcedata.codeTableName)
                                       ).
                                       
                  if resourcedata.NotInUse then 
                  do:
                      if resourcedata.tableName > "" then
                          edErrors = (if edErrors > "" then edErrors + "~n~n" else "") + subst("Table '&1' is not in use.",resourcedata.tableName).
                      else     
                          edErrors = (if edErrors > "" then edErrors + "~n~n" else "") + subst("Code Table '&1' is not in use.",resourcedata.codeTableName).
                  end.
                  // lazy way to do else on the last long expression
                  else if resourceData.noCodes = false then 
                      edErrors = (if edErrors > "" then edErrors + "~n~n" else "") + subst("The '&1' Table is not defined in any database.", resourcedata.tableName).   
                 
                 edErrors:bgcolor =  Theme:WarningBg.
                 edErrors:fgcolor =  Theme:WArningFg.

             end.
             else do:
                 edErrors = "".
                 edErrors:bgcolor = ?.  
                 edErrors:fgcolor = ?.   
             end.     
        end.        
        else 
        do: 
            assign
                fiClassname      = ""
                fiResource       = ""
                fiEntityName     = "" 
                fiBusinessEntity = ""
                fiIncludeFile    = "" 
                fiTempTableName  = "" 
                fiDataSource     = ""
                edQuery          = ""
                edErrors         = ""
              .
            edErrors:bgcolor = ?.  
            edErrors:fgcolor = ?.   
                
        end.
        display 
            fiClassName 
            fiResource 
            fiEntityName 
            fiBusinessEntity
            fiIncludeFile
            fiTempTableName
            fiDataSource
            edQuery
            edErrors.      
    end.
end.    

procedure DisplayResource :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
    
    define variable functions as ISet no-undo.
    define variable procedures as ISet no-undo.
    define variable sdo as SdoModel no-undo.
    define variable dlp as DlpModel no-undo.
    define variable datasource as DataSourceModel no-undo.
    define variable be as BusinessEntityModel no-undo.
    define variable dataFields as ISet no-undo.
   
    if avail resourceData then 
    do:
        dataFields = GetFields(table resourceData by-reference).
        if valid-object(resourceData.SdoModel) then 
        do:
            sdo = cast(resourceData.SdoModel,SdoModel).
        end.    
        if valid-object(resourceData.DlpModel) then 
        do:
            dlp = cast(resourceData.DlpModel,DlpModel).
        end.
        if not valid-object(resourceData.DataSourceModel) then do:
            resourceData.DataSourceModel = CreateDataSource(input table resourceData by-reference).
        end.
        if not valid-object(resourceData.BusinessEntityModel) then do:
            resourceData.BusinessEntityModel = CreateBusinessEntity(input table resourceData by-reference).
        end.
        be = cast(resourceData.BusinessEntityModel,BusinessEntityModel).  
        datasource = cast(resourceData.DataSourceModel,DataSourceModel).
            
    end.
    
    run Fillfields(dataFields,avail resourceData and resourceData.codeTableName = "").    
    run FillSdofunctions(if valid-object(sdo) then sdo:Functions else ?).
    run FillSdoProcedures(if valid-object(sdo) then sdo:Procedures else ?).
    run FillDlpfunctions(if valid-object(dlp) then dlp:Functions else ?).
    run FillDlpProcedures(if valid-object(dlp) then dlp:Procedures else ?).
    run FillDataSourceMethods(if valid-object(datasource) then datasource:methods  else ?).
    run FillBusinessEntityMethods(if valid-object(be) then be:methods  else ?).
    if valid-object(CodeTableModel) then
        CodeTableModel:SetCodeTablePosition(if avail resourceData then resourceData.codeTableName else "").
    if length(resourceData.tempTableName) > 32 then 
        resourceData.Error = AddTTLengthError(cast(resourceData.Error,ErrorTracker), resourceData.temptablename). 
    
    // do this last to allow code above to change resourcedata 
    run DisplayResourcefields(table resourceData by-reference).  
    run SensitizeResourceWidgets(table resourceData by-reference).   
end procedure.
    

procedure enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  display firectFilterTitle
          firectGenerateTitle
          
          fiBeMethodLabel
          fiDSMethodLabel
          fiErrorLabel
          fiQueryLabel
          fiDlpFuncLabel
          fiDlpProcLabel
          fiSdoFuncLabel
          fiSdoProcLabel 
          toSDOs
          toNoSDOs
          toDataFields
          toIncludes
          toDataSources
          toBusinessEntities
      with frame DEFAULT-FRAME in window C-Win.
     
  enable fiOutput 
         btnOutDir
         fiSourceOutput
         btnOutSourceDir 
         fiSDO 
         btnSdoDir 
         toSDOs
         toNoSDOs
         toDataFields
         toIncludes
         toDataSources
         toBusinessEntities
/*         toOverwriteInclude*/
/*         toOverwriteDS     */
/*         toOverwriteBE     */
         raGenerateAllorOne
         fiClassName
         fiResource
         fiEntityName
         fiBusinessEntity
         fiIncludefile
         fiTempTableName
         fiDataSource
         edErrors
         edQuery 
         businessEntityMethods
         dataSourceMethods
         sdoFunctions 
         sdoProcedures 
         dlpFunctions 
         dlpProcedures 
      with frame DEFAULT-FRAME in window C-Win.
      
    //  edErrors:read-only in frame default-frame = true.
    //  edQuery::read-only in frame default-frame = true. 
  
 // view C-Win.
end procedure.

procedure FillDlpFunctions :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    run FillMethods(pSet,dlpFunctions:handle in frame default-frame). 
end procedure.

procedure FillDlpProcedures :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    run FillMethods(pSet,dlpProcedures:handle in frame default-frame). 

end procedure.

procedure FillDataSourceMethods :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    run FillMethods(pSet,dataSourceMethods:handle in frame default-frame). 

end procedure.


procedure FillBusinessEntityMethods :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    run FillMethods(pSet,businessEntityMethods:handle in frame default-frame). 

end procedure.


procedure FillFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    define input  parameter pEnableIsExcluded as logical no-undo.
    
    define variable oIter  as IIterator no-undo.
    define variable oField as FieldModel no-undo.
    
    empty temp-table fieldData.    
     
    if valid-object(pSet) then                                                                                                                      
    do:                        
        oIter= pSet:Iterator().
        do while oIter:HasNext():
           create fieldData.
           oField = cast(OIter:Next(),FieldModel).
          
           fieldData.fieldName = ofield:name.
           fieldData.serializeName = oField:serializeName.
           fieldData.isCalc = oField:IsCalc.
           fieldData.dataType = oField:dataType.
           fieldData.isExcluded = fieldData.isCalc. 
           fieldData.inSDO = ofield:inSDO.
        end.
    end.                                                                                                                                            
    fieldBrowse:OpenQuery().
    if pEnableIsExcluded then fieldBrowse:EnableIsExcluded().
    else fieldBrowse:DisableIsExcluded().
end procedure.


procedure FillMethods :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input  parameter pSet as ISet no-undo.
    define input  parameter pwidget as handle no-undo.
    
    define variable oIter as IIterator no-undo.
    define variable oCode as CodeModel no-undo.
    
    pWidget:list-items = ?.
    
    if valid-object(pSet) then
    do:
        oIter= pSet:Iterator().
        do while oIter:HasNext():
           oCode = cast(OIter:Next(),CodeModel).
           pWidget:add-last(oCode:name). 
        end.    
    end.     
     
end procedure.


procedure FillSdoFunctions :
define input  parameter pSet as ISet no-undo.
 
    run FillMethods(pSet,sdoFunctions:handle in frame default-frame).  

end procedure.


procedure FillSdoProcedures :
define input  parameter pSet as ISet no-undo.
    run FillMethods(pSet,sdoProcedures:handle in frame default-frame).  
end procedure.


procedure Initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define variable hWin as handle no-undo.
   define variable oBar as IMonitorTarget   no-undo. 
   define variable iBar as integer no-undo.
   
   run Pmfo/Tools/Gui/winProgress.w persistent set hWin.
   Monitor = new Monitor().
   Extent(Monitor:Targets) = extent(resourceModel:MonitorSources).
   do iBar = 1 to extent(Monitor:Targets):
       run AddProgressBar in hWin(input resourceModel:MonitorSources[iBar], output oBar).
       Monitor:Targets[iBar] = oBar. 
   end. 
   // TODO why?? text disappears
   Monitor:Targets[1]:Text = resourceModel:MonitorSources[1].
       
   if not valid-object(Application:StartupManager) then 
   do:
       if superLib > "" then
          run value(superLib) persistent.
          
       StartupManager:Instance. /* sets Application:StartupManager in contructor */
   end.
   if not valid-object(Theme) then 
       Theme  = new DefaultTheme().  
   fieldBrowse:Theme = Theme.
   resourceBrowse:Theme = Theme.
   
   nameService = cast(Application:ServiceManager:getService(get-class(INameService)),INameService).   
  
   resourceModel:NameService = nameService.
   if valid-object(codeTableModel) then
   do:
       resourceModel:CodeTableModel = codeTableModel.
       resourceBrowse:CodeTableModel = codeTableModel.
       fieldBrowse:UnresolvedSerializeName = codeTableModel:UnresolvedSerializeName.
   end.
   
   run CreateCodeTableView.
    
   if not valid-object(CodeConverter) then
       CodeConverter = new CodeConverter().
   
   if not valid-object(CodeGeneratorClass) then
       CodeGeneratorClass = get-class(CodeGenerator).
   
   if fiOutput = "" then 
   do on error undo, throw:
       run SetApplicationFolder(GetServiceManager():EntityDirectory).
       catch e as Progress.Lang.Error :
          message subst("Could not find SessionManager:EntityDirectory '&1' in propath",GetServiceManager():EntityDirectory) 
          
          view-as alert-box.   
       end catch. 
   end.
         
   if fiSourceOutput = "" then    
   do on error undo, throw:
       run SetDataSourceFolder(GetServiceManager():DataSourceDirectory).
       catch e as Progress.Lang.Error :
          message subst("Could not find SessionManager:DataAccessDirectory '&1' in propath",GetServiceManager():EntityDirectory) 
          
          view-as alert-box.   
       end catch. 
   end.
   run fetchResources.    
   run refreshUI.      
   initialized = true.
   
   C-Win:hidden = false no-error.
   apply "close" to hWin.
   session:set-wait-state ("").
   
   // thown from IMonitorTarget when pressin Cancel Load in WinProgress 
   catch cancel as CancelError:
      if valid-handle(hWin) then
         apply "close" to hWin.
      apply "close" to this-procedure.
      
   end catch.
   catch e as Progress.Lang.Error :
         message e:GetMessage(1) skip
          e:Callstack
         view-as alert-box.  
         apply "close" to this-procedure.
   end catch.
   finally:
       session:set-wait-state ("").    
   end.    
end procedure.


procedure OutputDirchanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcDir as character no-undo.

end procedure.

procedure DataLayerOutputDirchanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcDir as character no-undo.

end procedure.

procedure OpenQuery:
    resourceBrowse:OpenQuery(toSDOs,toNoSDOs,toDataFields).
end.    

procedure SdoDirChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter psdoDir as character no-undo.
   session:set-wait-state ("general").
   run fetchResources.
   catch e as Progress.Lang.Error :
       undo, throw e.    
   end catch.
   finally:
       session:set-wait-state ("").
   end.    
end procedure.

procedure DlpFunctionSelected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oDlpModel as DLPModel no-undo.
    define variable cCode as longchar no-undo.
    
    oDlpModel = cast(ResourceData.DlpModel,DlpModel).
    run showCode(input-output dlpFuncWin,resourceData.dlpname,dlpFunctions:screen-value in frame default-frame,oDlpModel:Functions,"DLP","Function").

end procedure.

procedure DlpProcedureSelected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oDlpModel as DLPModel no-undo.
    define variable cCode as longchar no-undo.
    
    oDlpModel = cast(ResourceData.DlpModel,DlpModel).
    run showCode(input-output dlpProcWin,resourceData.dlpname,dlpProcedures:screen-value in frame default-frame,oDlpModel:Procedures,"DLP","Procedure").

end procedure.

procedure Generatecode:
    define variable cQuery as character no-undo.
    define variable oGenerator  as CodeGenerator no-undo.
    define variable hTable      as handle no-undo.
    define variable cFixedQuery as character no-undo init "resourceData.nocodes = false and resourceData.notInUse = false and valid-object(resourceData.Error) = false".
    define variable iWhere      as integer no-undo.
    define variable lEmergency  as logical no-undo.
    
    session:set-wait-state ("general").
    
    oGenerator = cast(CodeGeneratorClass:New(),CodeGenerator).
    oGenerator:DefinitionDir = DefinitionDir.
    /* goes to current if no path - sub paths are defined in CodeGenerator*/
    if fiOutput > "" then 
       oGenerator:ApplicationPath = fiOutput.
       
    if fiSourceOutput > "" then 
       oGenerator:DataLayerPath = fiSourceOutput.
        
    oGenerator:CodeTableModel = codeTableModel.
    
    cQuery = resourceBrowse:QueryExpression.
    ResourceBrowse:GetTable(output table-handle hTable by-reference). 
    if raGenerateAllorOne then
    do:
        cQuery = "for each resourceData where ".
        if resourceBrowse:QueryExpression > "" then 
        do:
            cQuery = cQuery + "(" + resourceBrowse:QueryExpression + ") and (" + cFixedQuery + ")". 
        end.        
        else
            cQuery = cQuery + cFixedQuery. 
        /* not sure if it is a good idea to sort, but */    
        if resourceBrowse:SortExpression > "" then
            cQuery = cQuery + " by " + resourceBrowse:SortExpression. 
        // deep copy in order to traverse without interruting browse- also because of Generate code with query current code need 
        //   it to pass buffer correctly
        lEmergency = false. 
        do on error undo, throw :
            oGenerator:Generate(cQuery, toIncludes, toDataSources, toBusinessEntities, fiOutput, input table-handle hTable,false ).
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 14631 then
                do:
                   lEmergency = AskEmergency(e,true). // true for plural message
                end.
                else 
                   message e:GetMessage(1)
                   view-as alert-box error.
            end catch.
         
        end.
        if lEmergency then do:
             oGenerator:Generate(cQuery, false, false, false, fiOutput, input table-handle hTable,true ).
        end.   
    end.
    else do:
        run GenerateCodeForSelectedRow(oGenerator, input table-handle hTable by-reference). 
    end.
    
    resourceBrowse:Display().
    
    finally:
        delete object oGenerator.
        session:set-wait-state ("").
      
    end.       
end.    

procedure GenerateCodeForSelectedRow:
    define input parameter pGenerator as CodeGenerator no-undo.
    define input parameter table for resourceData.
    define variable lOk        as logical no-undo.
    define variable lEmergency as logical no-undo.
    if not avail resourceData then
    do:
        message "No Record is selected"
            view-as alert-box info title "Cannot Generate Code" .
    end.   
    else do:
        lOk = true. 
        if resourceData.noCodes then 
        do:
            lOk = false.
            message subst("There is no data for Code Table '&1'.",resourceData.codeTableName) skip(1)
                    "Do you still want to generate code?"
            view-as alert-box question buttons yes-no title "Generate Code?" update lOk .
        end.    
        else if resourceData.notInUse then 
        do:
            lOk = false.
            message subst("The '&1' table is not in use.",resourceData.TableName) skip(1)
                    "Do you still want to generate code?"
            view-as alert-box question buttons yes-no title "Generate Code?" update lOk.
        end.    
        lEmergency = false. 
        if lOk then 
        do on error undo, throw:
            pGenerator:Generate(rowid(resourceData), if resourceData.onlyInclude then false else toIncludes, toDataSources, toBusinessEntities,  input table resourceData by-reference,false).
            
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 14631 then
                do:
                   lEmergency = AskEmergency(e,true). // true for plural message
                end.
                else 
                   message e:GetMessage(1)
                   view-as alert-box error. 
            end catch.
        end.
        // this will generate only includes withpout any json repository data 
        if lEmergency then 
        do: 
             pGenerator:Generate(rowid(resourceData), false,false,false, input table resourceData by-reference,true).
        end.      
    end.
    
end procedure.    

procedure SdoFunctionSelected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oSdoModel as SDOModel no-undo.
    define variable cCode as longchar no-undo.
    
    oSdoModel = cast(ResourceData.SdoModel,SdoModel).
    run showCode(input-output sdoFuncWin,resourceData.SDOname,sdoFunctions:screen-value in frame default-frame,oSdoModel:Functions,"SDO","Function").
  
    
end procedure.
    
procedure SdoProcedureSelected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oSdoModel as SDOModel no-undo.
    define variable cCode as longchar no-undo.
    
    oSdoModel = cast(ResourceData.SdoModel,SdoModel).
    run showCode(input-output sdoProcWin,resourceData.SDOname,sdoProcedures:screen-value in frame default-frame,oSdoModel:Procedures,"SDO","Procedure").

end procedure.
    
procedure DataSourceMethodSelected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oDataSource as DataSourceModel no-undo.
    define variable cCode as longchar no-undo.
    
    oDataSource = cast(ResourceData.DataSourceModel,DataSourceModel).
    run showCode(input-output dataSourceMethodWin,resourceData.dataSource,dataSourceMethods:screen-value in frame default-frame,oDataSource:Methods,"DataSource","Method").

end procedure.    

procedure BusinessEntityMethodSelected:
    define input parameter table for resourceData.
       
    define variable hTable as handle no-undo. 
    define variable oBusinessEntity as BusinessEntityModel no-undo.
    define variable cCode as longchar no-undo.
    
    oBusinessEntity = cast(ResourceData.BusinessEntityModel,BusinessEntityModel).
    run showCode(input-output businessEntityMethodWin,resourceData.businessEntity,businessEntityMethods:screen-value in frame default-frame,oBusinessEntity:Methods,"BusinesdEntity","Method").
    
end procedure.    
    
procedure SetCodeTableDataWindowName:
/*------------------------------------------------------------------------------
 Purpose: optional window to open from Show Data button for code table
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pWin as char no-undo.
   CodeTableDataWindowName = pWin.
   
end procedure.

procedure SetCodeTableView :
/*------------------------------------------------------------------------------
 Purpose: plug in optional view to display code table fields
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pCodeTableView as ICodeTableView no-undo.
   CodeTableView = pCodeTableView.
end procedure.

procedure SetCodeGeneratorClass :
/*------------------------------------------------------------------------------
 Purpose: plug in optional view to display code table fields
 Notes:
------------------------------------------------------------------------------*/
   define input parameter pGeneratorClass as Progress.Lang.Class no-undo.
   CodeGeneratorClass = pGeneratorClass.
end procedure.

procedure SetCodeTableModel :
/*------------------------------------------------------------------------------
 Purpose: plug in optional codetablemodel 
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcodetablemodel as ICodeTableModel no-undo.
   codeTableModel = pcodetablemodel.
end procedure.

procedure SetCodeConverter :
/*------------------------------------------------------------------------------
 Purpose: plug in optional codetablemodel 
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pconverter as CodeConverter no-undo.
   CodeConverter = pconverter.
end procedure.

procedure SetTheme :
/*------------------------------------------------------------------------------
 Purpose: plug in optional Theme
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pTheme as ITheme no-undo.
   Theme = pTheme.
end procedure.

procedure SetApplicationFolder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcFolder as character no-undo.
   define variable cParentPath as character   no-undo.
   assign cParentPath = getFullDirectoryPath(pcFolder).
   
   if cParentPath > "" then
   do:
       entry(num-entries(cParentPath,StringConstant:BACKSLASH),cParentPath,StringConstant:BACKSLASH) = "".
       cParentPath = right-trim(cParentPath,StringConstant:BACKSLASH).
       run SetApplicationOutputDirectory(cPArentPath).
   end.
   
end procedure.


procedure SetApplicationOutputDirectory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcdir as character no-undo.
   define variable cSegments as character extent no-undo.
   define variable cApp as character no-undo.
   assign fiOutput = getFullDirectoryPath(pcdir).
   if fiOutput > "" then
   do:
       cSegments = Array:Split(fiOutput,StringConstant:BACKSLASH).
       if extent(cSegments) > 1 then
       do:
           cApp = cSegments[Extent(cSegments) - 1].
           if cApp > "" then
              c-win:title = subst("&1 (&2)",cTitle,cApp).
       end.    
   end.
   display fiOutput with frame default-frame.
   if initialized then
       run OutputDirchanged(fiOutput).
end procedure.

procedure SetDataSourceFolder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcFolder as character no-undo.
   define variable cParentPath as character   no-undo.
   assign cParentPath = getFullDirectoryPath(pcFolder).
   
   if cParentPath > "" then
   do:
       entry(num-entries(cParentPath,StringConstant:BACKSLASH),cParentPath,StringConstant:BACKSLASH) = "".
       cParentPath = right-trim(cParentPath,StringConstant:BACKSLASH).
       run SetDataLayerOutputDirectory(cPArentPath).
   end.
end procedure.

procedure SetDataLayerOutputDirectory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcdir as character no-undo.
   assign fiSourceOutput = getFullDirectoryPath(pcdir).
   display fiSourceOutput with frame default-frame.
   if initialized then
       run DataLayerOutputDirchanged(fiSourceOutput).
end procedure.

procedure SetSuperLibrary :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcLib as character no-undo.
   assign superLib = pcLib.
end procedure.

procedure SetSdoDirectory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input  parameter pcdir as character no-undo.
   
   assign fiSDO = getFullDirectoryPath(pcdir).
   display fiSDO with frame default-frame.
   if initialized then
       run SdoDirChanged(fiSdo). 
end procedure.


procedure ShowCode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define input-output  parameter pWindow as handle no-undo.
   define input  parameter pcSourceName  as character no-undo.
   define input  parameter pcMethodName  as character no-undo.
   define input  parameter pMethods as ICollection no-undo.
   define input  parameter pcObjectType as char no-undo.
   define input  parameter pcMethodType as char no-undo.
   
   define variable procs as ISet no-undo.
   define variable cCode as longchar no-undo.
   define variable cDataType as character no-undo.
   if not valid-handle(pWindow) then 
   do:
       run Pmfo/Tools/Gui/WinCode.w persistent set pWindow.
   end.
   cCode = GetSelectedCode(pcMethodName, pMethods, output cdatatype).
   if cDataType > "" then 
       pcmethodname = pcMethodname + " RETURNS " + cDatatype.
   run SetContent in pWindow (pcObjectType + " " + pcSourceName,pcMethodType,pcMethodName,cCode).
   run MoveToTop in pWindow. 
   
end procedure.

procedure ShowCodeTableData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define input parameter table for resourceData.
    if not valid-handle(CodeTableDataWindow) then do:
        if CodeTableDataWindowName > "" then do: 
             
            run value(CodeTableDataWindowName) persistent set CodeTableDataWindow.
            
            run Initialize in CodeTableDataWindow.
            
            CodeTableModel:PositionChanged:subscribe(CodeTableDataWindow,"OpenQuery"). 
        end.    
    end.
    else
       run MoveToTop in CodeTableDataWindow.
    if valid-handle(CodeTableDataWindow) then   
        CodeTableModel:SetCodeTablePosition(resourceData.codeTableName).   
end procedure.

procedure CloseCodeTableFilter:
    if valid-handle(CodeTableSearchWindow) then 
       apply 'close' to CodeTableSearchWindow.
end.    

procedure ShowCodeTableFilter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    if not valid-handle(CodeTableSearchWindow) then do:
        if CodeTableSearchWindowName > "" then do:
             
            run value(CodeTableSearchWindowName) persistent set CodeTableSearchWindow.
            run SetResourceBrowse  in CodeTableSearchWindow (resourceBrowse).
            run Initialize in CodeTableSearchWindow.
        //    CodeTableModel:PositionChanged:subscribe(CodeTableDataWindow,"OpenQuery"). 
        end.
        else 
            message "The Code Generator Window is not configured for Code Table Search. Use SetCodeTableSearchWindowName to set the program to launch."
            view-as alert-box info.    
    end.
    else
       run MoveToTop in CodeTableSearchWindow.
    
/*    if valid-handle(CodeTableDataWindow) then                           */
/*        CodeTableModel:SetCodeTablePosition(resourceData.codeTableName).*/
end procedure.


procedure RefreshCodeTable:
    define input parameter table for resourceData.
    define input  parameter pmovetotop as logical no-undo.
    if valid-handle(CodeTableDataWindow) then do:
        run SetCodeTableKey in CodeTableDataWindow (resourceData.codeTableName,pmovetotop).
    end.  
end.    

procedure RefreshService:
     session:set-wait-state ("general").
     run Pmfo/Tools/AppBuilder\refreshResources.p.
     // not in interface - exists only for thos tool
     cast(Application:ServiceManager,ServiceManager):RefreshResources().
     resourceBrowse:Display().
     finally:
         session:set-wait-state ("").
     end.    
end.    

/* ************************  Function Implementations ***************** */

function AddError returns ErrorTracker 
    ( pErrorTracker as ErrorTracker,pEnum as ErrorTrackerEnum, piParam as char extent ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    if not valid-object(pErrorTracker) then
        pErrorTracker = new ErrorTracker(). 
    pErrorTracker:AddError(pEnum,piParam).
    return pErrorTracker.    
end function.

function AddTTLengthError returns ErrorTracker 
    ( pErrorTracker as ErrorTracker, pTTname as char ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
      return AddError(pErrorTracker, ErrorTrackerEnum:TooLongTempTableName, Array:ToArray(pTTname)).
end function.

function AskEmergency returns logical 
    ( perror as Error ,plMany  as log):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    define variable cMessage as character no-undo.
    define variable lAnswer  as logical no-undo.
    message 
      "The following error prevented code generation:"  skip(1)
      pError:GetMessage(1) skip(1)
      "This likely means that the Service Manager failed to start and Repository Management is not working."
      subst("Since this could be caused by code not compiling you may generate the definition&1",if plMany then "s" else "")
      skip(1)
      subst("Do you want to generate the defintion&1 for the selected resource&1?",if plMany then "s" else "")
    
    view-as alert-box warning buttons yes-no title "Emergency Question" update lAnswer.
    return lAnswer.    
end function.

function CreateBusinessEntity returns BusinessEntityModel
    ( input table resourceData ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable oModel as BusinessEntityModel no-undo.    
    define variable oErrorTracker as ErrorTracker no-undo.
    if valid-object(resourceData.Error) then
        oErrorTracker = cast(resourceData.Error,ErrorTracker).
    else     
        oErrorTracker = new ErrorTracker().
    oModel = CodeConverter:CreateBusinessEntity(resourceData.businessEntity,
                                              cast(resourceData.SdoModel,SdoModel), 
                                              cast(resourceData.DlpModel,DlpModel), 
                                              resourceData.resourceName,
                                              resourceData.tempTableName, 
                                              resourceData.beforeTableName,
                                              resourceData.includefile,
                                              oErrorTracker).  
    if not valid-object(resourceData.Error) and oErrorTracker:HasMessage() then
        resourceData.Error = oErrorTracker.
     
    return oModel.                                                  
end function.

function CreateDataSource returns DataSourceModel 
    ( input table resourceData ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    define variable oModel as DataSourceModel no-undo.
    define variable oErrorTracker as ErrorTracker no-undo.
    if valid-object(resourceData.Error) then
        oErrorTracker = cast(resourceData.Error,ErrorTracker).
    else     
        oErrorTracker = new ErrorTracker().
    oModel = CodeConverter:CreateDataSource(resourceData.dataSource, 
                                          cast(resourceData.SdoModel,SdoModel), 
                                          cast(resourceData.DlpModel,DlpModel), 
                                          resourcedata.tableName,
                                          resourceData.tempTableName, 
                                          resourceData.beforeTableName,
                                          resourceData.includefile,
                                          oErrorTracker).
    
    if not valid-object(resourceData.Error) and oErrorTracker:HasMessage() then
        resourceData.Error = oErrorTracker.
     
    return oModel.     
end function.

function GetBusinessEntityDirectory returns character 
    (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
        message "service man entity" GetServiceManager():EntityDirectory
        view-as alert-box.
    return getFullDirectoryPath(GetServiceManager():EntityDirectory).
end function.

function GetFields returns ISet 
    ( input table resourceDAta ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    
    define variable datafields as ISet no-undo.
    if avail resourceData then do:
        if valid-object(resourceData.SdoModel) then 
        do:
            if valid-object(resourceData.sdoFields) then
                datafields = cast(resourceData.SdoFields,ISet).          
        end.   
        else if valid-object(resourceData.dbFields) then
            datafields = cast(resourceData.dbFields,ISet).          
        else if resourceData.codeTableName > "" then 
        do:
            if not valid-object(resourcedata.codeTableFields) then
                resourcedata.codeTableFields = codeTableModel:GetCodeTableFields(resourceData.codeTableName).
            if valid-object(resourcedata.codeTableFields) then     
                datafields = cast(resourceData.codeTableFields,ISet).          
        end.
    end.
    return dataFields.
        
end function.

function getFullDirectoryPath returns character 
    ( pcdir as character ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
   file-info:file-name = pcdir.
   
   if not file-info:file-type matches "*D*" then
       message subst("&1 &2 failed. This is not a directory",entry(1,program-name(2),""),pcDir) 
       view-as alert-box warning. 
   if file-info:full-pathname = ? then
       
       message subst("&1 &2 failed. Directory does not exist",entry(1,program-name(2),""),pcDir) 
       view-as alert-box warning. 
   
   return file-info:full-pathname. 
        
end function.

function GetSdoDirectory returns character 
    (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

        define variable result as character no-undo.

        return result.


        
end function.

function GetSelectedCode returns longchar
  ( pcname as char, pMethods as ICollection, output pdatatype as char ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable oIterator as IIterator no-undo.
    define variable oCode     as Codemodel no-undo.
    oIterator = pMethods:Iterator().
    do while oIterator:HasNext():
        oCode = cast(oIterator:Next(),CodeModel).
        if ocode:name = pcName then 
        do:
            if type-of(oCode,FunctionModel) then 
                pDataType = cast(oCode,FunctionModel):DataType.
            return oCode:code.
        end.    
    end.
    return ?.
end function.

function GetServiceManager returns IServiceManager 
    (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
   return cast(Application:ServiceManager,IServiceManager).
end function.

function RemoveError returns ErrorTracker 
   ( pErrorTracker as ErrorTracker,pEnum as ErrorTrackerEnum, piParam as char extent ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    pErrorTracker:RemoveError(pEnum,piParam).
    if not pErrorTracker:HasMessage() then 
    do:
        delete object pErrorTracker. 
        return ?.
    end.
    else 
        return pErrorTracker.    
end function.

function RemoveTTLengthError returns ErrorTracker 
  ( pErrorTracker as ErrorTracker,pTTname as character):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
     return RemoveError(pErrorTracker, ErrorTrackerEnum:TooLongTempTableName, Array:ToArray(pTTname)).    
end function.

function GetErrorMessages returns longchar 
    ( perror as ErrorTracker ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    if valid-object(pError) then 
    do:
       return pError:GetMessages(2). // 2 number of line feeds per message      
    end.    
    return "".
end function.    



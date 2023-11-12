 
 /*------------------------------------------------------------------------
    File        : FileBrowse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 15 17:48:48 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Tools.Gui.View.Browse from propath.
using Pmfo.Tools.Gui.View.IView from propath.
using Progress.Lang.AppError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.
using Pmfo.Tools.Util.ColorManager from propath.
 
block-level on error undo, throw.

class Pmfo.Tools.Gui.View.ResourceBrowse inherits Browse: 
    {Pmfo/Tools/AppBuilder/resourcedata.i reference-only}
    
    define public property DefinitionDir as character no-undo get. set.
    
    define protected property ServiceManager as IServiceManager no-undo
        get:
            return cast(Application:ServiceManager,IServiceManager).
        end.     
     
    define query qResource for ResourceData scrolling.
    
    define browse bResource query qResource
        display ResourceData.resourceName   format "x(32)"
                ResourceData.businessEntity format "x(32)"
                resourceData.includefile    format "x(32)"
                resourceData.entityName     format "x(32)"
                resourceData.DataSource     format "x(32)"
                ResourceData.NotFromData    column-label "Src" view-as toggle-box //   Feel free to improve label
                  
                ResourceData.tableName      format "x(32)"
                ResourceData.tempdb         column-label "Temp" view-as toggle-box 
                resourceData.CodeTableKey   label "Code Key" // datatype and format  is defined in include with preprocessor 
                resourceData.CodeTableName  format "x(40)" 
                
                if ResourceData.SDOname > "" then entry(num-entries(ResourceData.SDOname,StringConstant:BACKSLASH),ResourceData.SDOname,StringConstant:BACKSLASH) else "" @ ResourceData.SDOname format "x(25)" // width 50 
                if ResourceData.dlpName > "" then entry(num-entries(ResourceData.dlpName,StringConstant:BACKSLASH),ResourceData.dlpName,StringConstant:BACKSLASH) else "" @ ResourceData.dlpName format "x(25)" // width 50 
               
        WITH NO-ROW-MARKERS SEPARATORS  
        18 DOWN  
        title "Data Artifacts"  .
         
    define frame Browse-Frame
      bResource    
      with
      three-d no-box
      no-underline scrollable  overlay.  
    
    
    define public property QueryString as char no-undo 
        get():
            if valid-handle(Browse:Query) then
                return Browse:Query:prepare-string.
            else
               return ?.    
        end get. 
    
    define public property CodeTableModel as ICodeTableModel no-undo 
        get.
        set(pModel as ICodeTableModel):
            CodeTableModel = pModel.
            SetLabelsFromModel().
        end.      
    
    constructor public ResourceBrowse (hParent as handle ):
        super (bResource:handle in frame Browse-Frame, hParent ).     
    end constructor.
    
    method public void OpenQuery( pSDOs as logical, pNoSDOs as logical, pDataFields as logical ):
        define variable cQuery as character no-undo.
        if pSDOs = false and pnosdos = false and pdatafields = false then
            cQuery = "false".
        else if pSDOs and pnosdos and pDataFields then
            cQuery  = "". // rese to empty 
        else if pSDOs and pnosdos then
            cQuery = "resourceData.CodeTableName = ''". 
        else do:
            if pSDOs then do:
                cQuery  = "(resourceData.sdoname > '' or resourceData.dlpname > '' )".
            end.    
            else if pNoSDOs then do:
                if cQuery > "" then cQuery = cQuery + " or ". 
                cQuery  = cQuery + "(resourceData.sdoname = '' and resourceData.dlpname = '' and resourceData.CodeTableName = '')".
            end.    
            if pDataFields then
            do:
                if cQuery > "" then cQuery = cQuery + " or ". 
                cQuery  = cQuery +  "resourceData.CodeTableName >  '' ".
            end.    
        end. 
        OpenQuery(cQuery,?).     
    
    end.
    
    method protected void SetLabelsFromModel():
        if valid-object(CodeTableModel) then
        do:
            resourceData.codeTableKey:label in browse bResource = CodeTableModel:CodeTableKeyLabel.
            resourceData.codeTableName:label in browse bResource = CodeTableModel:CodeTableNameLabel.
        end.
    end method.    
    
    method public override void Init():
        super:Init().
        SetLabelsFromModel().
    end method.    
    
    method public void SearchQuery( pView as IView ):
         define variable i as integer no-undo.
         // TODO - use a dedicate view/search class with wrap around and seatch forward and backward 
         define variable lWrapped as logical no-undo.
         define variable rRowid as rowid no-undo.
         if valid-object(CodeTableModel) then
         do: 
             if not avail resourceData then 
             do:
                 this-object:Query:get-first.
                 lWrapped = true.
             end.
             if avail resourceData then
             do: 
                 rRowid = rowid(resourceData).
                 do while avail resourceData:
                     // todo disambiguate find next and find first maybe..
                     this-object:Query:get-next.
                     
                     if CodeTableModel:IsMatch(pView, input table resourceData by-reference) then
                     do:
                         rRowid = rowid(resourceData).
                         leave.
                     end.       
                     
                     // back to start
                     if rRowid = rowid(resourceData) then leave.
                             
                     if not avail resourceData then
                     do:
                         if not lWrapped then
                         do:  
                             this-object:Query:get-first.
                             lWrapped = true.
                         end.
                     end.            
                 end.
                 if rRowid <> ? then
                 do: 
                     this-object:browse:Set-repositioned-row(integer(this-object:browse:down / 2),"CONDITIONAL").
                     this-object:Query:reposition-to-rowid(rowid(resourceData)).
                     FireValueChanged().
                 end.
             end.
             return.
         end.
         else 
             undo, throw new AppError("ResourceBrowse is not configured to searh. The CodeTableModel property is not set.",?).
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public void Bind(table ResourceData bind):
     
    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public void GetTable( output table ResourceData ):
    end method.

    method override protected void RowDisplay(  ):
        define variable lExists as logical no-undo.
        define variable lEntityExists as logical no-undo.
        define variable lDisabled as logical no-undo.
        if valid-object(Theme) then 
        do:
            if Resourcedata.NotFromData then
            do:
                 setRowColor(Theme:InfoFg,Theme:InfoBg).
            end. 
            else if // resourceData.noCodes or
                resourcedata.NotInUse or (resourcedata.tableName > "" and resourcedata.tableName <> CodeTableModel:CodeTableName and resourceData.definedinDb = false) then
            do: 
                setRowColor(Theme:DisabledRowFg,Theme:DisabledRowBg).
                lDisabled = true.
            end.      
            
            if ServiceManager:CanFindPublicEntity(input ResourceData.resourceName) then
            do:
                lExists = true.
                if lDisabled then
                do:
                    resourceData.resourcename:fgcolor in browse bResource = Theme:PublicDisabledFg.    
                    resourceData.resourcename:bgcolor in browse bResource = Theme:PublicDisabledBg.
                end.    
                else do:    
                    resourceData.resourcename:fgcolor in browse bResource = Theme:PublicFg.    
                    resourceData.resourcename:bgcolor in browse bResource = Theme:PublicBg.
                end.
                    
            end.    
            else if ServiceManager:CanFindEntity(input ResourceData.resourceName) then
            do:
                lExists = true.
                if lDisabled then
                do:
                    resourceData.resourcename:fgcolor in browse bResource = Theme:ProtectedDisabledBg.    
                    resourceData.resourcename:bgcolor in browse bResource = Theme:ProtectedDisabledBg.
                end.
                else do:
                    resourceData.resourcename:fgcolor in browse bResource = Theme:ProtectedFg.    
                    resourceData.resourcename:bgcolor in browse bResource = Theme:ProtectedBg.
                end.    
            end.
            if search(subst("&1/&2.i",DefinitionDir,resourceData.includefile)) <> ? then
            do:
                lExists = true.
                resourceData.includefile:fgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledFg else Theme:ExistsFg.    
                resourceData.includefile:bgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledBg else Theme:ExistsBg.    
            end.
            if search(subst("&1/&2.cls",ServiceManager:DataSourceDirectory,resourceData.dataSource)) <> ? then
            do:
                lExists = true.
                resourceData.dataSource:fgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledFg else Theme:ExistsFg.    
                resourceData.dataSource:bgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledBg else Theme:ExistsBg.    
            end.
            if search(subst("&1/&2.cls",ServiceManager:EntityDirectory,resourceData.businessEntity)) <> ? then
            do:
                lExists = true.
                resourceData.businessEntity:fgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledFg else Theme:ExistsFg.    
                resourceData.businessEntity:bgcolor in browse bResource = if lDisabled then Theme:ExistsDisabledBg else Theme:ExistsBg.    
            end.
            if resourceData.entityName > "" and ServiceManager:CanFindDataSource(resourceData.entityName) then
            do:
                lEntityExists = true.
                resourceData.entityName:fgcolor in browse bResource = if lDisabled then Theme:ProtectedDisabledFg else Theme:ProtectedFg.    
                resourceData.entityName:bgcolor in browse bResource = if lDisabled then Theme:ProtectedDisabledBg else Theme:ProtectedBg.    
            end.   
            if lExists then
            do:
                if not lEntityExists then
                do:
                    if resourceData.tableName = CodeTableModel:CodeTableName then
                    do:
                        resourceData.tableName:fgcolor in browse bResource = Theme:DisabledRowFg.    
                        resourceData.tableName:bgcolor in browse bResource = Theme:DisabledRowBg.    
                    end.
/*                    else do:                                                                        */
/*                        if resourceData.tableName > "" then                                         */
/*                        do:                                                                         */
/*                             resourceData.tableName:fgcolor in browse bResource = Theme:ErrorFg.    */
/*                             resourceData.tableName:bgcolor in browse bResource = Theme:ErrorBg.    */
/*                        end.                                                                        */
/*                        else if resourceData.codeTableName > "" then                                */
/*                        do:                                                                         */
/*                             resourceData.codeTableName:fgcolor in browse bResource = Theme:ErrorFg.*/
/*                             resourceData.codeTableName:bgcolor in browse bResource = Theme:ErrorBg.*/
/*                             resourceData.codeTableKey:fgcolor in browse bResource = Theme:ErrorFg. */
/*                             resourceData.codeTableKey:bgcolor in browse bResource = Theme:ErrorBg. */
/*                        end.                                                                        */
/*                    end.                                                                            */
                end.     
            end.   
        end.        
        super:RowDisplay().

    end method.
 
end class.
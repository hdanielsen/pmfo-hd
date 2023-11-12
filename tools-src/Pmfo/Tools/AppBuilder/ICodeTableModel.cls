
/*------------------------------------------------------------------------
    File        : ICodeTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 02 08:49:42 EST 2019
    Notes       : Interface that abstracts a code table allowing applications to 
                  implement a model that integrates the code table with 
                  WinDesign (Used by CodeGenerator and ResourceModel and more)
                  to generate temp-table definitions and data source info 
                  in resources.json as data sources allow code table data to 
                  appear as data from regular tables.
                - A code table is a generic table used to store lookup data 
                  where a type key is used as the logical table identifier
                - If the code table has a complicated type key with spaces or 
                  other characters that is not allowed in a regular table 
                  one also need to implement a NameUtil override with Nice*
                  overrides that converts the type key to something that can be 
                  used as temp-table name and collection name in json         
  ----------------------------------------------------------------------*/
 
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Tools.Gui.Model.IResourceModel from propath.
using Pmfo.Tools.Gui.View.IView from propath.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.

interface Pmfo.Tools.Appbuilder.ICodeTableModel:  
    {Pmfo/Tools/AppBuilder/resourcedata.i reference-only}
    
    // for serialization of index 
    define public property IndexAsString     as character extent no-undo get.  
    define public property CodeTableName     as character no-undo get.
    // intweger or character - decides whether to  call  
    // ResolveIntegerKeyFromQuery or ResolveCharacterKeyFromQuery 
    // integerkey is used to find with CodeTableKey 
    // character key is used to find with CodeTableName 
    define public property CodeTableKeyType  as char no-undo get.  
    
    // name to use as label
    define public property CodeTableKeyLabel        as char no-undo get.  
    define public property CodeTableNameLabel       as char no-undo get.
    
    
    /*
     serialize name to use for generic fields in the cocde table 
     It is common in cde tables to have generalpurpose fields like field1 field2 
     These should be named manually. This property defines a name to use
     to identify these fields in tools and  UI */  
    define public property UnresolvedSerializeName as char no-undo get.  
    
    
    /* to be published from SetCodeTablePosition so visual objects can subscribe and show data */
    define public event PositionChanged signature void (table-handle hBuffer).
    
    /* return a collection of the fields - as aminimimum value and name/description, 
       but many code tables have additional fields. Additional Fields with 
       generic names that are not suited for public names should have 
       serialize-name set to UnresolvedSerializeName    
     */
    method public ICollection GetCodeTableFields(pcKey as char).
    
      // for serialization of notes in comment 
    method public character GetCodeTableNotes(pcKey as char).
    
    // should set position (find the parent "type" table record) and publish 
    //  PositionChanged with a temp-table copy of the code type table record
    method public void SetCodeTablePosition(pcKey as char).
    
    /* create resource data records for each code type. should populate fields like 
                resourceData.codeTableName  = mycodetypekey 
                resourceData.className      = pResourceModel:NameUtil:ClassName(mycodetypekey,true) 
                resourceData.noCodes        = no data  - not can-find(first code of type).
                pResourceModel:AfterRow(input table resourceData by-reference).         
       One should typically extend the NameUtil Nice* methods to deal with difficult cases
       
       The WinDesign has apis to provide a INameUtil class          
                */ 
    method public void AddCodes(pResourceModel as IResourceModel ,input-output table resourceData ). 
    method public void AddCodes(pResourceModel as IResourceModel ,pMonitorTarget as IMonitorTarget,input-output table resourceData ). 
    
    // Pass the query from an SDO based on the code table and extract the key  
    // return ? for not found 
    method public integer   ResolveIntegerKeyFromQuery(pQuery as char).
    method public character ResolveCharacterKeyFromQuery(pQuery as char).
    method public void GenerateDataSourceConstructor(phStream as handle,input table resourceData).
    // passed from query/broiwse to check if search values in the view mathces current record 
    // The implementation should pass an iview implementation that the CodeTalbeModel implementation
    // understands.  
    method public logical  IsMatch(pView as IView,input table resourceData).
    
         
end interface.
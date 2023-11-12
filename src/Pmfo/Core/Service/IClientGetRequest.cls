
/*------------------------------------------------------------------------
    File        : IClientRequest
    Purpose     : Builds GetDataRequest from dataset handle using public names
    Syntax      : 
    Description : Defines a data request from public names   
    Author(s)   : hdaniels
    Created     : Sun Mar 31 15:46:35 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.

interface Pmfo.Core.Service.IClientGetRequest inherits IClientRequest:  
    define public property SkipProp          as character   no-undo get.  
    define public property NumRecordsProp    as character   no-undo get.  
    define public property SortProp          as character   no-undo get. 
    define public property FilterProp        as character   no-undo get. 
    define public property CountProp         as character   no-undo get. 
    define public property PagingContextProp as character   no-undo get. 
    define public property SelectProp        as character   no-undo get. 
    define public property LevelsProp        as character   no-undo get. 
    define public property ExpandProp        as character   no-undo get. 
    define public property TimeoutProp       as character   no-undo get. 
    define public property WildcardsProp     as character   no-undo get. 
    define public property ApplyProp         as character   no-undo get. 
    
    define public property UriQuery          as IStringStringMap no-undo get. 
    
    define public property Skip          as int64     no-undo get. 
    define public property NumRecords    as int64     no-undo get. 
    define public property Sort          as character no-undo get. 
    define public property Filter        as character no-undo get. 
    define public property Count         as logical   no-undo get. 
    define public property PagingContext as character no-undo get. 
    define public property Select        as character no-undo get. 
    define public property Levels        as integer   no-undo get. 
    define public property Expand        as character no-undo get. 
    define public property Timeout       as integer   no-undo get. 
    define public property Wildcards     as logical   no-undo get. 
    define public property Apply         as character no-undo get. 
    define public property GroupBy       as character no-undo get.
    define public property ApplyFilter   as character no-undo get.
    // optinally allow check for aggregate without creating aggregate collection  
    define public property HasAggregate  as logical no-undo get. 
    define public property Aggregates    as ICollection no-undo get. 
         
    define public property ExpandInnerJoins    as character no-undo get.
    define public property ApplyInnerJoins    as character no-undo get.
     
//    define public property Expand        as character no-undo get. 
 
    define public property MetaDataEnum  as ResponseMetaDataEnum no-undo get. 
    
    define public property ExpandedRelations as ICollection no-undo get. set.
    define public property ContextProperties as char extent no-undo get. 
    define public property CollectionProperties as char extent no-undo get. 
    define public property KeyProperties as char extent no-undo get. 
    
    method public void AssertUriQuery(pRequestType as RequestTypeEnum, phDataset as handle).  
    
    method public character GetExpandSelect (pcPathEntity as character).
    method public character GetChildFilter  (pcPathEntity as character).
    method public character GetChildSort    (pcPathEntity as character).   
end interface.
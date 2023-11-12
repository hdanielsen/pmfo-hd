
/*------------------------------------------------------------------------
    File        : IFieldManager
    Purpose     : Common interface for managing fields in db that are not in the attach for 
                  various reasonc   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Feb 13 12:53:43 EST 2022
    Notes       : Known classes/sub interfaces
                  
                - IFieldSaveManager 
                  This is a dynamic manager that works on fields that are defined as mapped from 
                  TT to Source and attached as defailt, but the data sometime is conditionally 
                  transformed for the client. 
                  The DataSource has a FieldManager property for this, but there is no default 
                  implementation. 
                  The Application may for example implement this for data conversion to and 
                  from metric/imperial data: 
                  When the data is transformed forem the source when serialized 
                  to the client the client will submit transformed before image 
                  values and to manage this we avoid attaching the fields 
                  for save   
                - ProxyManager 
                  Is used to present the data as a different data type than the Db source 
                  or derive normalized regaulr data field from overloaded database values 
                   (comma separated values or other markup tricks) 
               -  SourceFieldManager 
                  Used to map more or less constant values  that is not exposed to the UI at all, 
                  they are typcially also managed as fixed filters in a BaseQuery, but the field 
                  object (ISourceField) is needed for FindSource where clause and to assign vaiues 
                  to the db on create      
                - SaveUtil  
                  Is more of utility that is used instead of save-row-changes to for conflict and 
                  save tt field values when some of the datasource key fields are managed by 
                  the ProxyManager or SourceFieldManager and not attached  thus preventing use of 
                  save-row-changes
                - The posssible evolution of this is to merge all of these and stop 
                  using save-row-changes all together (but we need to attach for read
                  for performance and save of very many rfecords and/orlarge tables 
                  also benfits froma attahc and seve-roe-changex so this is not certain )                        
----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.

interface Pmfo.Core.DataLayer.IFieldManager:
    define public property HasKey as logical no-undo get.   
    define public property HasMandatoryInitNull as logical no-undo get.   
    
    // for spoirceWhere  - the dbbuffer is likely not needed but it is aclled from methods
    // that has this a parameter so may in theory be used for other buffer than UpdateTarget BufferHandle   
    method public character WhereExpression(phDbBuffer as handle, pcSourcename as character).
    method public logical   SaveRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle). 
    method public character CheckRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle).
end interface.
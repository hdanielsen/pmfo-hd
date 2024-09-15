
/*------------------------------------------------------------------------
    File        : IFindService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jun 12 00:52:15 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.

interface Pmfo.Core.BusinessLogic.IFindService:  
    method public character FindDescription(pcResource as character,pcKeyValue as character).
    method public character FindDescription(pcResource as character,piKeyValue as integer).
    method public character FindDescription(pcResource as character,pcFieldName as character,piKeyValue as integer ).
    method public character FindDescription(pcResource as character,pcFieldName as character,pdKeyValue as decimal).
    method public character FindDescription(pcResource as character,pcFieldName as character,pcKeyValue as character ).
    method public character FindDescription(pcResource as character,pcFieldNames as character extent, pcKeyValues as character extent ).
    method public character FindDescription(pcResource as character,pQueryEntry as IQueryEntry).
    
    method public character FindName(pcResource as character,pcKeyValue as character).
    method public character FindName(pcResource as character,pdKeyValue as decimal).
    method public character FindName(pcResource as character,piKeyValue as integer).
    method public character FindName(pcResource as character,pcFieldName as character,piKeyValue as integer ).
    method public character FindName(pcResource as character,pcFieldName as character,pdKeyValue as decimal).
    method public character FindName(pcResource as character,pcFieldName as character,pcKeyValue as character ).
    method public character FindName(pcResource as character,pcFieldNames as character extent, pcKeyValues as character extent ).  
    method public character FindName(pcResource as character,pQueryEntry as IQueryEntry).
    
    // throws not found error if not found
    method public void AssertFind(pcResource as character,piKeyValue as integer).
    method public void AssertFind(pcResource as character,pdKeyValue as decimal).
    method public void AssertFind(pcResource as character,pcKeyValue as character).
    method public void AssertFind(pcResource as character,pcKeyName as char, piKeyValue as integer).
    method public void AssertFind(pcResource as character,pcKeyName as char, pdKeyValue as decimal).
    method public void AssertFind(pcResource as character,pcKeyName as char, pcKeyValue as character).
    method public void AssertFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent).
    method public void AssertFind(pcResource as character,pQueryEntry as IQueryEntry).
    
    method public logical CanFind(pcResource as character,piKeyValue as integer).
    method public logical CanFind(pcResource as character,pdKeyValue as decimal).
    method public logical CanFind(pcResource as character,pcKeyValue as character).
    method public logical CanFind(pcResource as character,pcKeyName as char, piKeyValue as integer).
    method public logical CanFind(pcResource as character,pcKeyName as char, pdKeyValue as decimal).
    method public logical CanFind(pcResource as character,pcKeyName as char, pcKeyValue as character).
    method public logical CanFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent).
    method public logical CanFind(pcResource as character,pQueryEntry as IQueryEntry).
    
    //  can-find variation that returns the entity  
    method public logical CanFind(pcResource as character,piKeyValue as integer, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pdKeyValue as decimal, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pcKeyValue as character, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pcKeyName as char, piKeyValue as integer, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pcKeyName as char, pdKeyValue as decimal, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pcKeyName as char, pcKeyValue as character, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent, output pEntity as IBusinessEntity).
    method public logical CanFind(pcResource as character,pQueryEntry as IQueryEntry,output pEntity as IBusinessEntity).
    
    // can find first needs field name or query  
    method public logical CanFindFirst(pcResource as character,pcFieldName as char, piFieldValue as integer).
    method public logical CanFindFirst(pcResource as character,pcFieldName as char, pdFieldValue as decimal).
    method public logical CanFindFirst(pcResource as character,pcFieldName as char, pcFieldValue as character).
    method public logical CanFindFirst(pcResource as character,pcFieldNames as char extent, pcFieldValues as character extent).
    method public logical CanFindFirst(pcResource as character,pQueryEntry as IQueryEntry). 
     
    method public character FindValue(pcResource as character,piKeyValue as integer,cpFieldName as character).
    method public character FindValue(pcResource as character,pdKeyValue as decimal,cpFieldName as character).
    method public character FindValue(pcResource as character,pcKeyValue as character,cpFieldName as character).
    method public character FindValue(pcResource as character,pcKeyName as char, piKeyValue as integer,cpFieldName as character).
    method public character FindValue(pcResource as character,pcKeyName as char, pdKeyValue as decimal,cpFieldName as character).
    method public character FindValue(pcResource as character,pcKeyName as char, pcKeyValue as character,cpFieldName as character).
    method public character FindValue(pcResource as character,pcKeyNames as char extent, piKeyValues as integer extent,pcFieldName as character).
    method public character FindValue(pcResource as character,pcKeyNames as char extent, pdKeyValues as decimal extent,pcFieldName as character).
    method public character FindValue(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent,pcFieldName as character).
    method public character FindValue(pcResource as character,pQueryEntry as IQueryEntry,pcFieldName as character).
    
    method public character extent FindValues( pcResource as character, piKeyValue as integer, pcFieldNames as character extent ).
    method public character extent FindValues( pcResource as character, pdKeyValue as decimal, pcFieldNames as character extent ).
    method public character extent FindValues( pcResource as character, pcKeyValue as character, pcFieldNames as character extent ).
    
    method public character extent FindValues(pcResource as character,pcKeys as char extent, piKeyValues as integer extent,pcFieldNames as character extent).
    method public character extent FindValues(pcResource as character,pcKeys as char extent, pdKeyValues as decimal extent,pcFieldNames as character extent).
    method public character extent FindValues(pcResource as character,pcKeys as char extent, pcKeyValues as character extent,pcFieldNames as character extent).
   
    method public character extent FindValues(pcResource as character,pcKey as char, piKeyValue as integer,pcFieldNames as character extent).
    method public character extent FindValues(pcResource as character,pcKey as char, pdKeyValue as decimal,pcFieldNames as character extent).
    method public character extent FindValues(pcResource as character,pcKey as char, pcKeyValue as character,pcFieldNames as character extent).
    method public character extent FindValues(pcResource as character,pQueryEntry as IQueryEntry,pcFieldNames as character extent).
   
    method public logical FindTable( input pcResource as character, input piKeyValue as integer, output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pdKeyValue as decimal, output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyValue as character,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyFieldName as character , input piKeyValue as integer,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyFieldName as character , input pdKeyValue as decimal,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyFieldName as character , input pcKeyValue as character,output table-handle phTable ).
    method public logical FindTable( input pcResource as character, input pcKeyFieldNames as character extent, input piKeyValues as integer extent,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyFieldNames as character extent, input pdKeyValues as decimal extent,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pcKeyFieldNames as character extent, input pcKeyValues as character extent,output table-handle phTable).
    method public logical FindTable( input pcResource as character, input pQueryEntry as IQueryEntry, output table-handle phTable).
  
 //   method public handle FindBuffer( pcResource as character, pcKeys as char extent, piKeyValues as character extent).
 //   method public handle FindBuffer( pcResource as character, pcKey as char, pcKeyValue as character).
 //   method public handle FindBuffer( pcResource as character, pcKey as char, piKeyValue as integer).
    
  
/*    method public int64     FindNumericValue(pcResource as character,pcKeyValue as character,cpFieldName as character).               */
/*    method public int64     FindNumericValue(pcResource as character,piKeyValue as integer,cpFieldName as character).                 */
/*    method public int64     FindNumericValue(pcResource as character,pcKey as char, pcKeyValue as character,cpFieldName as character).*/
/*    method public int64     FindNumericValue(pcResource as character,pcKey as char, piKeyValue as integer,cpFieldName as character).  */
/*                                                                                                                                  */
/*    method public decimal   FindDecimalValue(pcResource as character,pcKeyValue as character,cpFieldName as character).               */
/*    method public decimal   FindDecimalValue(pcResource as character,piKeyValue as integer,cpFieldName as character).                 */
/*    method public decimal   FindDecimalValue(pcResource as character,pcKey as char, pcKeyValue as character,cpFieldName as character).*/
/*    method public decimal   FindDecimalValue(pcResource as character,pcKey as char, piKeyValue as integer,cpFieldName as character).  */
   
     
end interface.